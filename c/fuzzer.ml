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

module Var = Fuzzer_var
module Subject = Fuzzer_subject
module State = Fuzzer_state
module State_list = Travesty.T_list.On_monad (State.Monad)
module Action = Fuzzer_action

(** [make_rng seed] creates a splittable RNG;
    if [seed] is [Some s], [s] will be used as the RNG's seed,
    otherwise a low-entropy system-derived seed is used. *)
let make_rng : int option -> Splittable_random.State.t = function
  | Some seed -> Splittable_random.State.of_int seed
  | None      -> Splittable_random.State.create
                   (Random.State.make_self_init ())
;;

let int_type ~(is_atomic : bool) ~(is_global : bool) : Mini.Type.t =
  let module T = Mini.Type in
  let basic = if is_atomic then T.Basic.atomic_int else T.Basic.int in
  T.of_basic basic ~is_pointer:is_global
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
  [%expect {| ((Normal int) (Pointer_to int) (Normal atomic_int) (Pointer_to atomic_int)) |}]
;;

let random_index (srng : Splittable_random.State.t) (xs : 'a list)
  : int option =
  if List.is_empty xs
  then None
  else Some (Splittable_random.int srng ~lo:0 ~hi:(List.length xs - 1))
;;

let%test_unit "random_index is always in bounds" =
  let rng = Random.State.make_self_init ~allow_in_tests:true () in
  let srng = Splittable_random.State.create rng in
  Core_kernel.Quickcheck.test
    ~shrinker:(List.shrinker Int.shrinker)
    ~sexp_of:[%sexp_of: int list]
    (List.gen Int.gen)
    ~f:(
      [%test_pred: int list] ~here:[[%here]]
        (fun xs ->
           match random_index srng xs with
           | None -> List.is_empty xs
           | Some i -> Option.is_some (List.nth xs i)
        )
    )
;;

(** [random_item srng xs] behaves like
    {{!List.random_element}List.random_element}, but uses a splittable
    RNG for compatibility with Quickcheck etc. *)
let random_item (srng : Splittable_random.State.t) (xs : 'a list)
  : 'a option =
  Option.(random_index srng xs >>= List.nth xs)
;;

let%expect_test "random item: empty list" =
  let deterministic_srng = Splittable_random.State.of_int 0 in
  Sexp.output_hum stdout
    [%sexp (random_item deterministic_srng [] : int option)];
  [%expect {| () |}]
;;

let%test_unit "random_item is always a valid item" =
  let rng = Random.State.make_self_init ~allow_in_tests:true () in
  let srng = Splittable_random.State.create rng in
  Core_kernel.Quickcheck.test
    ~shrinker:(List.shrinker Int.shrinker)
    ~sexp_of:[%sexp_of: int list]
    (List.gen Int.gen)
    ~f:(
      [%test_pred: int list] ~here:[[%here]]
        (fun xs ->
           match random_item srng xs with
           | None -> List.is_empty xs
           | Some x -> List.mem ~equal:Int.equal xs x
        )
    )
;;

(** [gen_int32_as_int] generates an [int] whose domain is that of
    [int32].  This is useful for making sure that we don't generate
    integers that could overflow when running tests on 32-bit
    platforms.  *)
let gen_int32_as_int : int Quickcheck.Generator.t =
  Quickcheck.Generator.map ~f:(fun x -> Option.value ~default:0 (Int.of_int32 x)) Int32.gen
;;

(** Shorthand for stating that a fuzzer action is always available. *)
let always : Subject.Test.t -> bool State.Monad.t =
  Fn.const (State.Monad.return true)

(** Fuzzer action that generates a new global variable. *)
module Make_global : Action.S = struct
  module Random_state = struct
    type t =
      { is_atomic : bool
      ; initial_value : int
      ; name : C_identifier.t
      }

    module G = Quickcheck.Generator

    let gen' (vars : Var.Map.t)
      : t G.t =
      let open G.Let_syntax in
      let%bind is_atomic = G.bool in
      let%bind initial_value = gen_int32_as_int in
      let%map  name = Var.Map.gen_fresh_var vars in
      { is_atomic; initial_value; name }

    let gen (_subject : Subject.Test.t) : t G.t State.Monad.t =
      State.Monad.with_vars gen'
  end

  let available = always

  let run
      (subject : Subject.Test.t)
      ({ is_atomic; initial_value; name } : Random_state.t)
    : Subject.Test.t State.Monad.t =
    let open State.Monad.Let_syntax in
    let ty = int_type ~is_atomic ~is_global:true in
    let%map () = State.Monad.register_global ty name
        ~initial_value:(Var.Value.Int initial_value)
    in
    let const = Mini.Constant.Integer initial_value in
    Subject.Test.add_var_to_init subject name const
  ;;
end

(** Fuzzer action that stores a constant value to an atomic variable. *)
module Constant_store : Action.S = struct
  (* TODO(@MattWindsor91): the actual chosen generated global should
     go in here as well. *)
  module Random_state = struct
    type t =
      { new_value : int
      ; global    : C_identifier.t
      ; path      : Mini.stm_hole Mini.program_path
      }

    module G = Quickcheck.Generator

    let gen' (subject : Subject.Test.t) (vars : Var.Map.t) : t G.t =
      let open G.Let_syntax in
      let%bind new_value = gen_int32_as_int in
      let%bind global    = Var.Map.random_generated_atomic_global vars in
      let%map  path      = Subject.Test.Path.gen_insert_stm subject in
      { new_value; global; path }
    ;;

    let gen (subject : Subject.Test.t) : t G.t State.Monad.t =
      State.Monad.with_vars (gen' subject)
    ;;
  end

  let available = fun _ ->
    State.Monad.with_vars Var.Map.has_generated_atomic_global
  ;;

  let run (subject : Subject.Test.t)
      ( { new_value; global; path } : Random_state.t)
    : Subject.Test.t State.Monad.t =
    let open State.Monad.Let_syntax in
    let src =
      Mini.Expression.constant (Mini.Constant.Integer new_value)
    in
    let dst =
      Mini.Address.lvalue (Mini.Lvalue.variable global)
    in
    (* TODO(@MattWindsor91): memory models. *)
    let mo = Mem_order.Relaxed in
    let constant_store =
      Mini.Statement.atomic_store
        (Mini.Atomic_store.make ~src ~dst ~mo)
    in
    let%bind () = State.Monad.erase_var_value global in
    State.Monad.Monadic.return (
      Subject.Test.Path.insert_stm path constant_store subject
    )
  ;;
end

let generate_random_state
  (type rs)
  (module Act : Action.S with type Random_state.t = rs)
  (subject : Subject.Test.t)
  (rng : Splittable_random.State.t)
  : rs State.Monad.t =
  let open State.Monad.Let_syntax in
  let%map gen = Act.Random_state.gen subject in
  Quickcheck.Generator.generate gen rng ~size:10
;;

let run_action
  (module Act : Action.S)
  (subject : Subject.Test.t)
  (rng : Splittable_random.State.t)
  : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let%bind state = generate_random_state (module Act) subject rng in
  Act.run subject state
;;

let table : Action.List.t Lazy.t =
  lazy
    (Weighted_list.from_alist_exn
       [ (module Make_global    : Action.S), 1
       ; (module Constant_store : Action.S), 2
       ]
    )
;;

let mutate_subject
    (subject : Subject.Test.t)
    (rng : Splittable_random.State.t)
  : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let cap = 10 in
  let table = Lazy.force table in
  let%map (_, subject') =
    State.Monad.fix
        (cap, subject)
        ~f:(fun mu (remaining, subject') ->
            if Int.(remaining = 0)
            then return (remaining, subject')
            else
              let%bind action =
                Action.List.pick table subject' rng
              in
              let%bind subject'' = run_action action subject' rng in
              mu (remaining - 1, subject'')
          )
  in subject'
;;

let run_with_state
    (test : Mini_litmus.Ast.Validated.t)
    (rng  : Splittable_random.State.t)
  : Mini_litmus.Ast.Validated.t State.Monad.t =
  let open State.Monad.Let_syntax in
  (* TODO: add uuid to this *)
  let name = Mini_litmus.Ast.Validated.name test in
  let post = Mini_litmus.Ast.Validated.post test in
  let subject = Subject.Test.of_litmus test in
  let%bind subject' = mutate_subject subject rng in
  State.Monad.with_vars_m
    (fun vars ->
       State.Monad.Monadic.return
         (Subject.Test.to_litmus ~vars ~name ?post subject')
    )
;;

(** [get_first_func test] tries to get the first function in a
   validated litmus AST [test]. *)
let get_first_func
    (test : Mini_litmus.Ast.Validated.t)
  : Mini.Function.t Or_error.t =
  let open Or_error.Let_syntax in
  (* If this is a validated litmus test, it _should_ have at least
     one function, and each function _should_ report the right
     global variables. *)
  let%map (_name, func) =
    test
    |> Mini_litmus.Ast.Validated.programs
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
    (test : Mini_litmus.Ast.Validated.t)
  : Mini.Type.t C_identifier.Map.t Or_error.t =
  Or_error.(
    test
    |>  get_first_func
    >>| Mini.Function.parameters
    >>= C_identifier.Map.of_alist_or_error
  )
;;

let make_initial_state
    (test : Mini_litmus.Ast.Validated.t)
  : State.t Or_error.t =
  let open Or_error.Let_syntax in
  let all_cvars = Mini_litmus.cvars test in
  let%map globals = existing_globals test in
  let global_cvars =
    globals |> C_identifier.Map.keys |> C_identifier.Set.of_list in
  let locals =
    C_identifier.Set.diff all_cvars global_cvars
  in
  State.init globals locals
;;

let run ~(seed : int option)
    (test : Mini_litmus.Ast.Validated.t)
  : Mini_litmus.Ast.Validated.t Or_error.t =
  Or_error.(
    make_initial_state test
    >>= State.Monad.run (run_with_state test (make_rng seed))
  )
;;
