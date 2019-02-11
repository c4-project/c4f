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

  let try_extract_parameter_type
    ((n, var) : C_identifier.t * Var.Record.t)
    : (C_identifier.t * Mini.Type.t) Or_error.t =
    Or_error.(
      var
      |> Var.Record.ty
      |> Result.of_option
        ~error:(Error.of_string "Internal error: missing global type")
      >>| Tuple2.create n
    )
  ;;

  (** [make_function_parameters vars] creates a uniform function
      parameter list for a C litmus test using the global
      variable records in [vars]. *)
  let make_function_parameters
    (vars : Var.Map.t)
    : Mini.Type.t Mini.id_assoc Or_error.t =
    vars
    |> C_identifier.Map.filter ~f:(Var.Record.is_global)
    |> C_identifier.Map.to_alist
    |> List.map ~f:try_extract_parameter_type
    |> Or_error.combine_errors
  ;;

  (** [to_function vars prog_id prog] lifts a subject-program [prog]
      with ID [prog_id]
      back into a Litmus function, adding a parameter list generated
      from [vars]. *)
  let to_function
      (vars : Var.Map.t)
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
      (vars : Var.Map.t)
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
      ~(vars : Var.Map.t)
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
    (subject : Subject.t)
    : Subject.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let ty = int_type ~is_atomic ~is_global:true in
  let%map var =
    State.Monad.gen_and_register_fresh_var ty
      ~initial_value:(Var.Value.Known_int initial_value)
  in
  let const = Mini.Constant.Integer initial_value in
  Subject.add_var_to_init subject var const
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

let on_random_existing_program (subject : Subject.t)
    ~(f : Subject_program.t -> Subject_program.t State.Monad.t)
  : Subject.t State.Monad.t =
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
  (prog : Subject_program.t)
  (stm  : Mini.Statement.t)
  : Subject_program.t State.Monad.t =
  let stms = prog.stms in
  State.Monad.with_rng
    (fun rng ->
       let stms' = insert_randomly rng stm stms in
       { prog with stms = stms' }
    )
;;

let make_constant_store_on
  (prog : Subject_program.t)
  (new_value : int)
  (global : C_identifier.t)
  : Subject_program.t State.Monad.t =
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

let run_action (subject : Subject.t)
  : Action.Payload.t -> Subject.t State.Monad.t = function
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
  State.Monad.with_vars_m
    (fun vars ->
       State.Monad.Monadic.return
         (Subject.to_litmus ~vars ~name ?post subject')
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
