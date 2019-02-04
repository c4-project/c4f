(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

module Var_record = struct
  type t =
    | Existing
    | Generated of Mini.Type.t

  (** [make_existing_var_map] expands a set of known-existing C variable
      names to a var-record map where each name is registered as
      an existing variable. *)
  let make_existing_var_map (existing_vars : C_identifier.Set.t)
    : t C_identifier.Map.t =
    C_identifier.Set.to_map existing_vars ~f:(Fn.const Existing)
  ;;
end

(** A stripped-down version of the validated C litmus test AST,
    used in fuzzing to avoid repeatedly having to validate. *)
module Subject = struct
  type t =
    { init     : Mini.Constant.t Mini.id_assoc
    ; programs : Mini.Function.t Mini.id_assoc
    }

  (** [of_litmus test] converts a validated C litmus test [test]
      to the intermediate form used for fuzzing. *)
  let of_litmus (test : Mini.Litmus_ast.Validated.t) : t =
    { init     = Mini.Litmus_ast.Validated.init test
    ; programs = Mini.Litmus_ast.Validated.programs test
    }
  ;;

  (** [to_litmus ?post subject ~name] tries to reconstitute a
      validated C litmus test from the subject [subject], attaching
      the name [name] and optional postcondition [post].  It may fail
      if the resulting litmus is invalid---generally, this signifies
      an internal error. *)
  let to_litmus
      ?(post:Mini.Litmus_ast.Post.t option)
      (subject : t)
      ~(name:C_identifier.t)
    : Mini.Litmus_ast.Validated.t Or_error.t =
    Mini.Litmus_ast.Validated.make
      ?post
      ~name
      ~init:(subject.init)
      ~programs:(subject.programs)
      ()
  ;;

  (** [add_var_to_init subject var initial_value] adds [var] to
      [subject]'s init block with the initial value [initial_value]. *)
  let add_var_to_init
      (subject : t)
      (var : C_identifier.t)
      (initial_value : Mini.Constant.t)
    : t =
    { subject with init = (var, initial_value) :: subject.init }
  ;;
end

module State = struct
  type t =
    { rng  : Splittable_random.State.t
    ; vars : Var_record.t C_identifier.Map.t
    }
  [@@deriving fields]
  ;;


  let init
      (rng : Splittable_random.State.t)
      (existing_vars : C_identifier.Set.t)
    : t =
    let vars = Var_record.make_existing_var_map existing_vars in
    { rng ; vars }
  ;;

  let register_var_direct
      (var : C_identifier.t) (ty : Mini.Type.t) (s : t) : t =
    { s with vars =
               C_identifier.Map.set s.vars
                 ~key:var ~data:(Generated ty)
    }
  ;;

  (** [gen_var_raw rng] generates a random C identifier, in
      string format, using [rng] as the RNG. *)
  let gen_var_raw (rng : Splittable_random.State.t) : string =
    let module Q = Quickcheck.Generator in
    sprintf "%c%d"
      (Q.generate ~size:0 Q.char_alphanum rng)
      (Q.generate ~size:5 Q.small_non_negative_int rng)
  ;;

  let%expect_test "gen_var_raw: example" =
    let deterministic_rng = Splittable_random.State.of_int 0 in
    print_string (gen_var_raw deterministic_rng);
    [%expect {| O0 |}]
  ;;

  (** [gen_var rng] generates a random C identifier, in
     {{!C_identifier.t}C_identifier.t} format, using [rng] as the
     RNG. *)
  let gen_var (rng : Splittable_random.State.t) : C_identifier.t =
    (* Assuming that [gen_var_raw] produces valid C identifiers by
       construction. *)
    C_identifier.of_string (gen_var_raw rng)
  ;;

  module Monad = Travesty.State_transform.Make (struct
      module Inner = Or_error
      type nonrec t = t
    end)
  ;;

(*
  let with_rng_m (f : Splittable_random.State.t -> 'a Monad.t)
    : 'a Monad.t =
    Monad.(peek rng >>= f)
  ;;
*)

  (** [with_rng f] is a variant of {{!with_rng_m}with_rng_m} which
      maps across [f] rather than binding. *)
  let with_rng (f : Splittable_random.State.t -> 'a)
    : 'a Monad.t =
    Monad.(peek rng >>| f)
  ;;


  (** [register_var var ty] is a stateful action that registers
      a generated variable [var] of type [ty] into the state,
      overwriting any existing variable of the same name. *)
  let register_var (var : C_identifier.t) (ty : Mini.Type.t)
    : unit Monad.t =
    Monad.modify (register_var_direct var ty)

  (** [gen_fresh_var ()] is a stateful action that generates a
      variable not already registered in the state (but doesn't
      register it itself). *)
  let gen_fresh_var () : C_identifier.t Monad.t =
    let open Monad.Let_syntax in
    let%bind rng  = Monad.peek rng in
    let%map  vars = Monad.peek vars in
    let rec mu () =
      let var = gen_var rng in
      if C_identifier.Map.mem vars var then mu () else var
    in mu ()
  ;;

  (** [gen_and_register_fresh_var ty] is a stateful action that
     generates a variable name not already
     registered in the state, then registers it as a generated
     variable of type [ty]. *)
  let gen_and_register_fresh_var (ty : Mini.Type.t)
    : C_identifier.t Monad.t =
    Monad.(gen_fresh_var () >>= tee_m ~f:(Fn.flip register_var ty))
  ;;
end

(** High-level actions that the mutator can take. *)
module Action = struct
  type t =
    | Make_global of { is_atomic : bool; initial_value : int }
    | Make_constant_store of { new_value : int }

  module Quickcheck = struct
    (*
    let anonymise = function
    | Make_global { is_atomic; initial_value } -> `A ((is_atomic, initial_value))
    | Make_store  { new_value : int } -> `B new_value
    ;;
       *)

    let deanonymise = function
    | `A ((is_atomic, initial_value)) -> Make_global { is_atomic; initial_value }
    | `B new_value -> Make_constant_store { new_value }
    ;;

    module G = Quickcheck.Generator
    let gen : t G.t =
      G.map ~f:deanonymise
        (
          Quickcheck.Generator.variant2
            (* Make_global *) (G.tuple2 G.bool Int.gen)
            (* Make_store  *) (Int.gen)
        )
  end

  include Quickcheck
end

(** [pick_action ()] is a stateful computation that randomly picks a
   new fuzzing action. *)
let pick_action () : Action.t State.Monad.t =
  State.with_rng
    (Quickcheck.Generator.generate ~size:10 Action.gen)
;;

(** [make_rng seed] creates a splittable RNG;
    if [seed] is [Some s], [s] will be used as the RNG's seed,
    otherwise a low-entropy system-derived seed is used. *)
let make_rng : int option -> Splittable_random.State.t = function
  | Some seed -> Splittable_random.State.of_int seed
  | None      -> Splittable_random.State.create
                   (Random.State.make_self_init ())
;;

let int_type ~(is_atomic : bool) : Mini.Type.t =
  Mini.Type.(
    normal (if is_atomic then atomic_int else int)
  )
;;

let make_global ~(is_atomic : bool) (initial_value : int)
    (subject : Subject.t)
    : Subject.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let ty = int_type ~is_atomic in
  let%map var = State.gen_and_register_fresh_var ty in
  let const =  Mini.Constant.Integer initial_value in
  Subject.add_var_to_init subject var const
;;

(* TODO(@MattWindsor91): implement this *)
let make_constant_store _new_value = State.Monad.return

let run_action
  : Action.t -> Subject.t -> Subject.t State.Monad.t = function
  | Make_global { is_atomic; initial_value } ->
    make_global ~is_atomic initial_value
  | Make_constant_store { new_value } ->
    make_constant_store new_value
;;

let mutate_subject (subject : Subject.t)
  : Subject.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let%bind action = pick_action () in
  (** TODO(@MattWindsor91): actually fuzz here *)
  run_action action subject
;;

let run_with_state (test : Mini.Litmus_ast.Validated.t)
  : Mini.Litmus_ast.Validated.t State.Monad.t =
  let open State.Monad.Let_syntax in
  (* TODO: add uuid to this *)
  let name = Mini.Litmus_ast.Validated.name test in
  let post = Mini.Litmus_ast.Validated.post test in
  let subject = Subject.of_litmus test in
  let%bind subject' = mutate_subject subject in
  State.Monad.Monadic.return
    (Subject.to_litmus ~name ?post subject')
;;

let run ~(seed : int option) (test : Mini.Litmus_ast.Validated.t)
  : Mini.Litmus_ast.Validated.t Or_error.t =
  let rng = make_rng seed in
  let existing_variables = Mini.litmus_cvars test in
  let initial_state = State.init rng existing_variables in
  State.Monad.run (run_with_state test) initial_state
;;
