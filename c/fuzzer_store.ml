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

(* Module shorthands *)
module Action  = Fuzzer_action
module Subject = Fuzzer_subject
module State   = Fuzzer_state
module Var     = Fuzzer_var

type rst =
  { store : Mini.Atomic_store.t
  ; path  : Mini_path.stm_hole Mini_path.program_path
  }
;;

module Int : Action.S with type Random_state.t = rst = struct
  let name = "store-int"

  (** Lists the restrictions we put on source variables. *)
  let src_restrictions : (Var.Record.t -> bool) list Lazy.t =
    lazy []
  ;;

  (** Lists the restrictions we put on destination variables. *)
  let dst_restrictions : (Var.Record.t -> bool) list Lazy.t =
    lazy
      Var.Record.[ is_atomic
                 ; was_generated
                   (* This is to make sure that we don't change the
                      observable semantics of the program over its
                      original variables. *)
                 ; Fn.non has_dependencies
                   (* This action changes the value, so we can't do it
                      to variables with depended-upon values. *)
                 ]
  ;;

  module Random_state = struct
    type t = rst

    let of_tuple (store, path) = { store; path }

    module G = Quickcheck.Generator

    (* TODO(@MattWindsor91): move this to Atomic_store. *)

    let src_env (vars : Var.Map.t) : (module Mini_env.S) =
      let predicates = Lazy.force src_restrictions in
      Var.Map.env_module_satisfying_all ~predicates vars
    ;;

    let dst_env (vars : Var.Map.t) : (module Mini_env.S) =
      let predicates = Lazy.force dst_restrictions in
      Var.Map.env_module_satisfying_all ~predicates vars
    ;;

    let error_if_empty (env : string) (module M : Mini_env.S) : unit Or_error.t =
      if Mini.Identifier.Map.is_empty M.env
      then Or_error.error_s
          [%message "Internal error: Environment was empty."
              ~here:[%here]
              ~env
          ]
      else Result.ok_unit
    ;;

    let gen_store
        (vf : Base.Formatter.t)
        (vars : Var.Map.t)
      : Mini.Atomic_store.t G.t Or_error.t =
      let (module Src) = src_env vars in
      let (module Dst) = dst_env vars in
      Fmt.pf vf "%s: got environments@." name;
      let open Or_error.Let_syntax in
      let%bind () = error_if_empty "src" (module Src) in
      let%map  () = error_if_empty "dst" (module Dst) in
      Fmt.pf vf "%s: environments are non-empty@." name;
      Fmt.pf vf "%s: src environment: @[%a@]@." name Sexp.pp_hum [%sexp (Src.env : Mini_type.t Utils.C_identifier.Map.t)];
      Fmt.pf vf "%s: dst environment: @[%a@]@." name Sexp.pp_hum [%sexp (Dst.env : Mini_type.t Utils.C_identifier.Map.t)];
      let module Gen = Mini.Atomic_store.Quickcheck_ints (Src) (Dst) in
      Fmt.pf vf "%s: built generator module@." name;
      [%quickcheck.generator: Gen.t]
    ;;

    let gen'
        (vf : Base.Formatter.t)
        (subject : Subject.Test.t)
        (vars : Var.Map.t)
      : t G.t Or_error.t =
      let open Or_error.Let_syntax in
      Fmt.pf vf "%s: building generators...@." name;
      let%map store = gen_store vf vars in
      Fmt.pf vf "%s: built store generator@." name;
      let path = Subject.Test.Path.gen_insert_stm subject in
      Fmt.pf vf "%s: built path generator@." name;
      G.map ~f:of_tuple (G.tuple2 store path)
    ;;

    let gen (subject : Subject.Test.t) : t G.t State.Monad.t =
      let open State.Monad.Let_syntax in
      let%bind vf = State.Monad.vf () in
      State.Monad.with_vars_m
        (Fn.compose State.Monad.Monadic.return
           (gen' vf subject))
    ;;
  end

  let available _ =
    State.Monad.with_vars (
      Var.Map.exists_satisfying_all
        ~predicates:(Lazy.force dst_restrictions)
    )
  ;;

  (* This action writes to the destination, so we no longer have a
     known value for it. *)
  let erase_value_of_store_dst (store : Mini.Atomic_store.t)
    : unit State.Monad.t =
    let dst = Mini.Atomic_store.dst store in
    let dst_var = Mini.Address.variable_of dst in
    State.Monad.erase_var_value dst_var
  ;;

  module Exp_idents =
    Mini.Expression.On_identifiers.On_monad (State.Monad)
  ;;

  (* This action also introduces dependencies on every variable in
     the source. *)
  let add_dependencies_to_store_src (store : Mini.Atomic_store.t)
    : unit State.Monad.t =
    Exp_idents.iter_m (Mini.Atomic_store.src store)
      ~f:State.Monad.add_dependency
  ;;

  let run (subject : Subject.Test.t)
      ( { store; path } : Random_state.t)
    : Subject.Test.t State.Monad.t =
    let open State.Monad.Let_syntax in
    let store_stm = Mini.Statement.atomic_store store in
    let%bind vf = State.Monad.vf () in
    Fmt.pf vf "%s: Erasing known value of store destination@." name;
    let%bind () = erase_value_of_store_dst store in
    Fmt.pf vf "%s: Adding dependency to store source@." name;
    let%bind () = add_dependencies_to_store_src store in
    State.Monad.Monadic.return (
      Subject.Test.Path.insert_stm path store_stm subject
    )
  ;;
end

module Int_test = struct
  let init : Mini.Constant.t Mini.id_assoc Lazy.t =
    lazy
      Mini.[ ( Identifier.of_string "x"
             , Constant.Integer 27
             )
           ; ( Identifier.of_string "y"
             , Constant.Integer 53
             )
           ]

  let globals : Mini.Type.t Mini.id_assoc Lazy.t =
    lazy
      Mini.[ ( Identifier.of_string "x"
             , Type.(pointer_to Basic.atomic_int)
             )
           ; ( Identifier.of_string "y"
             , Type.(pointer_to Basic.atomic_int)
             )
           ]
  ;;

  let body_stms : Mini.Statement.t list Lazy.t =
    lazy
      Mini.[ Statement.atomic_store
               (Atomic_store.make
                  ~src:(Expression.constant (Constant.Integer 42))
                  ~dst:(Address.of_variable (Identifier.of_string "x"))
                  ~mo:Mem_order.Seq_cst
               )
           ; Statement.nop ()
           ; Statement.atomic_store
               (Atomic_store.make
                  ~src:(Expression.lvalue (Lvalue.variable (Identifier.of_string "foo")))
                  ~dst:(Address.of_variable (Identifier.of_string "y"))
                  ~mo:Mem_order.Relaxed
               )
           ]

  let programs : Fuzzer_subject.Program.t list Lazy.t =
    let open Lazy.Let_syntax in
    let%bind parameters = globals   in
    let%map  body_stms  = body_stms in
    Mini.[
      Fuzzer_subject.Program.of_function
        (Function.make ~parameters ~body_decls:[] ~body_stms ())
    ]
  ;;

  let test_subject : Fuzzer_subject.Test.t Lazy.t =
    let open Lazy.Let_syntax in
    let%bind init     = init in
    let%map  programs = programs in
    { Fuzzer_subject.Test.init; programs }
  ;;

  let path : Mini_path.stm_hole Mini_path.program_path Lazy.t =
    lazy
      Mini_path.(
        On_program
          { index = 0
          ; rest = On_statements (Insert_at 2)
          }
      )
  ;;

  let store : Mini.Atomic_store.t Lazy.t =
    lazy
      Mini.(
        Atomic_store.make
          ~src:(Expression.atomic_load
                (Atomic_load.make ~src:(Address.of_variable (Identifier.of_string "gen2"))
                   ~mo:Mem_order.Seq_cst))
          ~dst:(Address.of_variable (Identifier.of_string "gen1"))
          ~mo:Mem_order.Seq_cst
      )

  let random_state : Int.Random_state.t Lazy.t =
    let open Lazy.Let_syntax in
    let%bind store = store in
    let%map  path  = path in
    { store; path }
  ;;

  let prepare_fuzzer_state () : unit Fuzzer_state.Monad.t =
    Fuzzer_state.Monad.(
      register_global
        Mini_type.(pointer_to Basic.atomic_int)
        (Mini.Identifier.of_string "gen1")
        ~initial_value:(Fuzzer_var.Value.Int 1337)
      >>= fun () ->
      register_global
        Mini_type.(pointer_to Basic.atomic_int)
        (Mini.Identifier.of_string "gen2")
        ~initial_value:(Fuzzer_var.Value.Int (-55))
    )
  ;;

  let init_fuzzer_state : Fuzzer_state.t Lazy.t =
    let open Lazy.Let_syntax in
    let%map globals_alist = globals in
    let globals = Mini.Identifier.Map.of_alist_exn globals_alist in
    Fuzzer_state.init ~globals ~locals:Mini.Identifier.Set.empty ()

  let run_test () : (Fuzzer_state.t * Fuzzer_subject.Test.t) Or_error.t =
    Fuzzer_state.Monad.(
      run'
        ( prepare_fuzzer_state ()
          >>= fun () -> Int.run (Lazy.force test_subject) (Lazy.force random_state)
        )
        (Lazy.force init_fuzzer_state)
    )
  ;;

  let%expect_test "test int store: programs" =
    let r =
      (let open Or_error.Let_syntax in
       let%bind (state, test) = run_test () in
       let vars = Fuzzer_state.vars state in
       let prog_results =
         List.mapi test.programs
           ~f:(fun id p ->
               p
               |>  Fuzzer_subject.Program.to_function ~vars ~id
               >>| Tuple2.uncurry Mini_reify.func
             )
       in
       Or_error.combine_errors prog_results
      )
    in
    Fmt.(
      pr "%a@."
        (result
           ~ok:(list Ast.External_decl.pp)
           ~error:Error.pp
        )
    ) r;
    [%expect {|
      void P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
      } |}]
  ;;

  let%expect_test "test int store: global variables" =
    let result =
      Or_error.(
        run_test ()
        >>| fst
        >>| Fuzzer_state.vars_satisfying_all
          ~predicates:[Fuzzer_var.Record.is_global]
      )
    in
    Sexp.output_hum stdout
      [%sexp (result : Mini.Identifier.t list Or_error.t)];
    [%expect {| (Ok (gen1 gen2 x y)) |}]
  ;;

  let%expect_test "test int store: variables with known values" =
    let result =
      Or_error.(
        run_test ()
        >>| fst
        >>| Fuzzer_state.vars_satisfying_all
          ~predicates:[Fuzzer_var.Record.has_known_value]
      )
    in
    Sexp.output_hum stdout
      [%sexp (result : Mini.Identifier.t list Or_error.t)];
    [%expect {| (Ok (gen2)) |}]
  ;;

  let%expect_test "test int store: variables with dependencies" =
    let result =
      Or_error.(
        run_test ()
        >>| fst
        >>| Fuzzer_state.vars_satisfying_all
          ~predicates:[Fuzzer_var.Record.has_dependencies]
      )
    in
    Sexp.output_hum stdout
      [%sexp (result : Mini.Identifier.t list Or_error.t)];
    [%expect {| (Ok (gen2)) |}]
  ;;
end
