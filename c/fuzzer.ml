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

(** [pick_action ()] is a stateful computation that randomly picks a
   new fuzzing action. *)
let pick_action () : Action.Payload.t State.Monad.t =
  State.Monad.with_rng
    (Quickcheck.Generator.generate ~size:10 Action.Payload.gen)
;;

(** [pick_actions n] is a stateful computation that randomly picks [n]
   fuzzing actions. *)
let pick_actions (num : int) : Action.Payload.t list State.Monad.t =
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
  let module T = Mini.Type in
  let basic = if is_atomic then T.atomic_int else T.int in
  let lift  = if is_global then T.pointer_to else T.normal in
  lift basic
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
    (subject : Subject.Test.t)
    : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let ty = int_type ~is_atomic ~is_global:true in
  let%map var =
    State.Monad.gen_and_register_fresh_var ty
      ~initial_value:(Var.Value.Known_int initial_value)
  in
  let const = Mini.Constant.Integer initial_value in
  Subject.Test.add_var_to_init subject var const
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

let on_random_existing_program (subject : Subject.Test.t)
    ~(f : Subject.Program.t -> Subject.Program.t State.Monad.t)
  : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  State.Monad.with_rng_m
    (fun rng ->
       let progs = subject.programs in
       let%map progs' =
         match random_index rng progs with
         | None -> State.Monad.return progs
         | Some i ->
           State_list.mapi_m progs
             ~f:(fun j -> if i = j then f else State.Monad.return)
       in
       { subject with programs = progs' }
    )

let all_generated_atomic_globals
    ()
  : C_identifier.t list State.Monad.t =
  State.Monad.with_vars (
    fun vars ->
      vars
      |> C_identifier.Map.filter
        ~f:(Travesty.T_fn.conj
              Var.Record.is_global
              (Travesty.T_fn.conj
                 Var.Record.is_atomic
                 Var.Record.was_generated
              )
           )
      |> C_identifier.Map.keys
  )
;;

let with_random_generated_atomic_global
  ~(f : C_identifier.t -> 'a State.Monad.t)
  ~(default : 'a State.Monad.t)
  : 'a State.Monad.t =
  let open State.Monad.Let_syntax in
  State.Monad.with_rng_m
    (fun rng ->
       let%bind globals = all_generated_atomic_globals () in
       match random_item rng globals with
       | None -> default
       | Some global -> f global
    )
;;

(** [insert_randomly srng x xs] inserts [x] into [xs] at a location
    determined by the RNG [srng]. *)
let insert_randomly
  (srng : Splittable_random.State.t)
  (x : 'a)
  (xs : 'a list)
  : 'a list =
  let i_opt = random_index srng (x :: xs) in
  let i = Option.value ~default:0 i_opt in
  let lhs, rhs = List.split_n xs i in
  lhs @ (x :: rhs)
;;

(** [insert_statement_randomly prog stm] is a stateful computation that
    injects [stm] into [prog]'s statement list at a random location.
    The state monad's random number generator decides where. *)
let insert_statement_randomly
  (prog : Subject.Program.t)
  (stm  : Mini.Statement.t)
  : Subject.Program.t State.Monad.t =
  let stms = prog.stms in
  State.Monad.with_rng
    (fun rng ->
       let stms' = insert_randomly rng stm stms in
       { prog with stms = stms' }
    )
;;

let make_constant_store_on
  (prog : Subject.Program.t)
  (new_value : int)
  (global : C_identifier.t)
  : Subject.Program.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let src =
    Mini.Expression.constant (Mini.Constant.Integer new_value)
  in
  let dst =
    Mini.Address.lvalue (Mini.Lvalue.variable global)
  in
  (* TODO(@MattWindsor91): memory models. *)
  let mo = Mem_order.Relaxed in
  let constant_store = Mini.Statement.atomic_store ~src ~dst ~mo in
  let%bind () = State.Monad.erase_var_value global in
  insert_statement_randomly prog constant_store
;;

(* TODO(@MattWindsor91): implement this *)
let make_constant_store (new_value : int) =
  on_random_existing_program
    ~f:(fun prog ->
        with_random_generated_atomic_global
          ~default:(State.Monad.return prog)
          ~f:(make_constant_store_on prog new_value)
      )
;;

let run_action (subject : Subject.Test.t)
  : Action.Payload.t -> Subject.Test.t State.Monad.t = function
  | Make_global { is_atomic; initial_value } ->
    make_global ~is_atomic initial_value subject
  | Make_constant_store { new_value } ->
    make_constant_store new_value subject
;;

let mutate_subject (subject : Subject.Test.t)
  : Subject.Test.t State.Monad.t =
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
  let subject = Subject.Test.of_litmus test in
  let%bind subject' = mutate_subject subject in
  State.Monad.with_vars_m
    (fun vars ->
       State.Monad.Monadic.return
         (Subject.Test.to_litmus ~vars ~name ?post subject')
    )
;;

(** [get_first_func test] tries to get the first function in a
   validated litmus AST [test]. *)
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
