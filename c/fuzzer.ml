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
    { ty : Mini.Type.t option
    ; source : [ `Existing | `Generated ]
    ; scope : [ `Global | `Local ]
    }
  ;;

  (** [is_global vr] returns whether [vr] is a global variable. *)
  let is_global : t -> bool = function
    | { scope=`Global ; _ } -> true
    | { scope=`Local  ; _ } -> false
  ;;

  (** [make_existing_var_map globals locals] expands a set of
     known-existing C variable names to a var-record map where each
     name is registered as an existing variable.

      Global registrations override local ones, in the case of
      shadowing. *)
  let make_existing_var_map
    (globals : Mini.Type.t C_identifier.Map.t)
    (locals  : C_identifier.Set.t)
    : t C_identifier.Map.t =
    let globals_map = C_identifier.Map.map globals
        ~f:(fun ty -> { ty = Some ty
                      ; source = `Existing
                      ; scope = `Global
                      })
    in
    let locals_map = C_identifier.Set.to_map locals
        ~f:(fun _ -> { ty = None
                     ; source = `Existing
                     ; scope = `Local
                     })
    in
    C_identifier.Map.merge globals_map locals_map
      ~f:(fun ~key -> ignore key;
           function
           | `Left x | `Right x | `Both (x, _) -> Some x
         )
end

module Subject_program = struct
  type t =
    { decls : Mini.Initialiser.t Mini.id_assoc
    ; stms  : Mini.Statement.t list
    }
  ;;

  let of_function (func : Mini.Function.t) : t =
    { decls = Mini.Function.body_decls func
    ; stms  = Mini.Function.body_stms func
    }
  ;;

  (** [make_function_parameters vars] creates a uniform function
      parameter list for a C litmus test using the global
      variable records in [vars]. *)
  let make_function_parameters
    (vars : Var_record.t C_identifier.Map.t)
    : Mini.Type.t Mini.id_assoc Or_error.t =
    vars
    |> C_identifier.Map.filter ~f:(Var_record.is_global)
    |> C_identifier.Map.to_alist
    |> List.map ~f:
      (function
        | (n, { Var_record.ty = Some t; _ }) -> Or_error.return (n, t)
        | _ -> Or_error.error_string
                 "Internal error: missing global type"
      )
    |> Or_error.combine_errors
  ;;

  (** [to_function vars prog_id prog] lifts a subject-program [prog]
      with ID [prog_id]
      back into a Litmus function, adding a parameter list generated
      from [vars]. *)
  let to_function
      (vars : Var_record.t C_identifier.Map.t)
      (prog_id : int)
      (prog : t) : Mini.Function.t Mini.named Or_error.t =
    let open Or_error.Let_syntax in
    let name = C_identifier.of_string (sprintf "P%d" prog_id) in
    let%map parameters = make_function_parameters vars in
    let func =
      Mini.Function.make
        ~parameters
        ~body_decls:prog.decls
        ~body_stms:prog.stms
        ()
    in (name, func)
  ;;
end

(** A stripped-down version of the validated C litmus test AST,
    used in fuzzing to avoid repeatedly having to validate. *)
module Subject = struct
  type t =
    { init     : Mini.Constant.t Mini.id_assoc
    ; programs : Subject_program.t list
    }
  ;;

  let programs_of_litmus (test : Mini.Litmus_ast.Validated.t)
    : Subject_program.t list =
    test
    |> Mini.Litmus_ast.Validated.programs
    |> List.map ~f:(fun (_, p) -> Subject_program.of_function p)
  ;;

  (** [of_litmus test] converts a validated C litmus test [test]
      to the intermediate form used for fuzzing. *)
  let of_litmus (test : Mini.Litmus_ast.Validated.t) : t =
    { init     = Mini.Litmus_ast.Validated.init test
    ; programs = programs_of_litmus test
    }
  ;;

  let programs_to_litmus
      (vars : Var_record.t C_identifier.Map.t)
    : Subject_program.t list ->
      Mini.Litmus_lang.Program.t list Or_error.t =
    Travesty.T_list.With_errors.mapi_m
      ~f:(Subject_program.to_function vars)
  ;;

  (** [to_litmus ?post subject ~vars ~name] tries to reconstitute a
     validated C litmus test from the subject [subject], attaching the
     name [name] and optional postcondition [post], and using the
     variable map [vars] to reconstitute parameters. It may fail if
     the resulting litmus is invalid---generally, this signifies an
     internal error. *)
  let to_litmus
      ?(post : Mini.Litmus_ast.Post.t option)
      (subject : t)
      ~(vars : Var_record.t C_identifier.Map.t)
      ~(name : string)
    : Mini.Litmus_ast.Validated.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind programs = programs_to_litmus vars subject.programs in
    Mini.Litmus_ast.Validated.make
      ?post
      ~name
      ~init:(subject.init)
      ~programs
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
      (globals : Mini.Type.t C_identifier.Map.t)
      (locals  : C_identifier.Set.t)
    : t =
    let vars =
      Var_record.make_existing_var_map globals locals in
    { rng ; vars }
  ;;

  let register_global_direct
      (var : C_identifier.t) (ty : Mini.Type.t) (s : t) : t =
    { s with vars =
               C_identifier.Map.set s.vars
                 ~key:var ~data:{ scope = `Global
                                ; ty = Some ty
                                ; source = `Generated
                                }
    }
  ;;

  (** [gen_var_raw rng] generates a random C identifier, in
      string format, using [rng] as the RNG. *)
  let gen_var_raw (rng : Splittable_random.State.t) : string =
    let module Q = Quickcheck.Generator in
    sprintf "%c%d"
      (Q.generate ~size:0 Q.char_alpha rng)
      (Q.generate ~size:5 Q.small_non_negative_int rng)
  ;;

  let%expect_test "gen_var_raw: example" =
    let deterministic_rng = Splittable_random.State.of_int 0 in
    print_string (gen_var_raw deterministic_rng);
    [%expect {| P0 |}]
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


  (** [register_global var ty] is a stateful action that registers
      a generated variable [var] of type [ty] into the state,
      overwriting any existing variable of the same name. *)
  let register_global (var : C_identifier.t) (ty : Mini.Type.t)
    : unit Monad.t =
    Monad.modify (register_global_direct var ty)

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
    Monad.(gen_fresh_var () >>= tee_m ~f:(Fn.flip register_global ty))
  ;;
end

module State_list = Travesty.T_list.On_monad (State.Monad)

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

(** [pick_actions n] is a stateful computation that randomly picks [n]
   fuzzing actions. *)
let pick_actions (num : int) : Action.t list State.Monad.t =
  num
  |> List.init ~f:(Fn.const (pick_action ()))
  |> State_list.sequence_m
;;

(** [make_rng seed] creates a splittable RNG;
    if [seed] is [Some s], [s] will be used as the RNG's seed,
    otherwise a low-entropy system-derived seed is used. *)
let make_rng : int option -> Splittable_random.State.t = function
  | Some seed -> Splittable_random.State.of_int seed
  | None      -> Splittable_random.State.create
                   (Random.State.make_self_init ())
;;

let int_type ~(is_atomic : bool) ~(is_global : bool) : Mini.Type.t =
  Mini.Type.(
    let basic = if is_atomic then atomic_int else int in
    let lift  = if is_global then pointer_to else normal in
    lift basic
  )
;;

let%expect_test "int_type: combinatoric" =
  Sexp.output_hum stdout
    [%sexp
      ( [ int_type ~is_atomic:false ~is_global:false
        ; int_type ~is_atomic:false ~is_global:true
        ; int_type ~is_atomic:true  ~is_global:false
        ; int_type ~is_atomic:true  ~is_global:true
        ] : Mini.Type.t list
      )
    ];
  [%expect {| ((Normal Int) (Pointer_to Int) (Normal Atomic_int) (Pointer_to Atomic_int)) |}]
;;

let make_global ~(is_atomic : bool) (initial_value : int)
    (subject : Subject.t)
    : Subject.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let ty = int_type ~is_atomic ~is_global:true in
  let%map var = State.gen_and_register_fresh_var ty in
  let const =  Mini.Constant.Integer initial_value in
  Subject.add_var_to_init subject var const
;;

(* TODO(@MattWindsor91): implement this *)
let make_constant_store _new_value = State.Monad.return

let run_action (subject : Subject.t)
  : Action.t -> Subject.t State.Monad.t = function
  | Make_global { is_atomic; initial_value } ->
    make_global ~is_atomic initial_value subject
  | Make_constant_store { new_value } ->
    make_constant_store new_value subject
;;

let mutate_subject (subject : Subject.t)
  : Subject.t State.Monad.t =
  let open State.Monad.Let_syntax in

  let%bind actions = pick_actions 10 in

  (** TODO(@MattWindsor91): actually fuzz here *)
  State_list.fold_m ~init:subject ~f:(run_action) actions
;;

let run_with_state (test : Mini.Litmus_ast.Validated.t)
  : Mini.Litmus_ast.Validated.t State.Monad.t =
  let open State.Monad.Let_syntax in
  (* TODO: add uuid to this *)
  let name = Mini.Litmus_ast.Validated.name test in
  let post = Mini.Litmus_ast.Validated.post test in
  let subject = Subject.of_litmus test in
  let%bind subject' = mutate_subject subject in
  let%bind vars = State.Monad.peek State.vars in
  State.Monad.Monadic.return
    (Subject.to_litmus ~vars ~name ?post subject')
;;

let get_first_func
    (test : Mini.Litmus_ast.Validated.t)
  : Mini.Function.t Or_error.t =
  let open Or_error.Let_syntax in
  (* If this is a validated litmus test, it _should_ have at least
     one function, and each function _should_ report the right
     global variables. *)
  let%map (_name, func) =
    test
    |> Mini.Litmus_ast.Validated.programs
    |> List.hd
    |> Result.of_option
      ~error:(
        Error.of_string
          "Internal error: validated litmus had no functions"
      )
  in func
;;

(** [existing_globals test] extracts the existing global variable
    names and types from litmus test [test]. *)
let existing_globals
    (test : Mini.Litmus_ast.Validated.t)
  : Mini.Type.t C_identifier.Map.t Or_error.t =
  Or_error.(
    test
    |>  get_first_func
    >>| Mini.Function.parameters
    >>= C_identifier.Map.of_alist_or_error
  )
;;

let make_initial_state
    (seed : int option)
    (test : Mini.Litmus_ast.Validated.t)
  : State.t Or_error.t =
  let open Or_error.Let_syntax in
  let rng = make_rng seed in
  let all_cvars = Mini.litmus_cvars test in
  let%map globals = existing_globals test in
  let global_cvars =
    globals |> C_identifier.Map.keys |> C_identifier.Set.of_list in
  let locals =
    C_identifier.Set.diff all_cvars global_cvars
  in
  State.init rng globals locals
;;

let run ~(seed : int option)
    (test : Mini.Litmus_ast.Validated.t)
  : Mini.Litmus_ast.Validated.t Or_error.t =
  Or_error.(
    make_initial_state seed test
    >>= State.Monad.run (run_with_state test)
  )
;;
