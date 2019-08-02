(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%test_module "int tests" =
  ( module struct
    open Act_fuzz
    open Store_actions

    let init : Act_c_mini.Constant.t Act_c_mini.Named.Alist.t Lazy.t =
      lazy
        [ (Act_common.C_id.of_string "x", Act_c_mini.Constant.int 27)
        ; (Act_common.C_id.of_string "y", Act_c_mini.Constant.int 53) ]

    let globals : Act_c_mini.Type.t Act_c_mini.Named.Alist.t Lazy.t =
      lazy
        Act_c_mini.
          [ ( Act_common.C_id.of_string "x"
            , Type.(int ~pointer:true ~atomic:true ()) )
          ; ( Act_common.C_id.of_string "y"
            , Type.(int ~pointer:true ~atomic:true ()) ) ]

    let body_stms : Act_c_mini.Statement.t list Lazy.t =
      lazy
        Act_c_mini.
          [ Statement.atomic_store
              (Atomic_store.make
                 ~src:(Expression.constant (Act_c_mini.Constant.int 42))
                 ~dst:(Address.of_variable (Act_common.C_id.of_string "x"))
                 ~mo:Mem_order.Seq_cst)
          ; Statement.nop ()
          ; Statement.atomic_store
              (Atomic_store.make
                 ~src:
                   (Expression.lvalue
                      (Lvalue.variable (Act_common.C_id.of_string "foo")))
                 ~dst:(Address.of_variable (Act_common.C_id.of_string "y"))
                 ~mo:Mem_order.Relaxed) ]

    let programs : Subject.Program.t list Lazy.t =
      let open Lazy.Let_syntax in
      let%bind parameters = globals in
      let%map body_stms = body_stms in
      Act_c_mini.
        [ Subject.Program.of_function
            (Function.make ~parameters ~body_decls:[] ~body_stms ()) ]

    let test_subject : Subject.Test.t Lazy.t =
      let open Lazy.Let_syntax in
      let%bind init = init in
      let%map programs = programs in
      {Subject.Test.init; programs}

    let path : Act_c_mini.Path_shapes.program Lazy.t =
      lazy Act_c_mini.Path_shapes.(in_func 0 (in_stms (insert 2)))

    let store : Act_c_mini.Atomic_store.t Lazy.t =
      lazy
        Act_c_mini.(
          Atomic_store.make
            ~src:
              (Expression.atomic_load
                 (Atomic_load.make
                    ~src:
                      (Address.of_variable
                         (Act_common.C_id.of_string "gen2"))
                    ~mo:Mem_order.Seq_cst))
            ~dst:(Address.of_variable (Act_common.C_id.of_string "gen1"))
            ~mo:Mem_order.Seq_cst)

    let random_state : Int.Payload.t Lazy.t =
      let open Lazy.Let_syntax in
      let%bind store = store in
      let%map path = path in
      Random_state.make ~store ~path

    let prepare_fuzzer_state () : unit State.Monad.t =
      State.Monad.(
        register_global
          Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
          (Act_common.C_id.of_string "gen1")
          ~initial_value:(Act_c_mini.Constant.int 1337)
        >>= fun () ->
        register_global
          Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
          (Act_common.C_id.of_string "gen2")
          ~initial_value:(Act_c_mini.Constant.int (-55)))

    let init_fuzzer_state : State.t Lazy.t =
      Lazy.Let_syntax.(
        let%map globals_alist = globals in
        let vars =
          globals_alist
          |> List.map ~f:(fun (id, ty) ->
                 ( Act_common.Litmus_id.global id
                 , Act_fuzz.Var.Record.make_existing Global (Some ty) ))
          |> Map.of_alist_exn (module Act_common.Litmus_id)
          |> Act_common.Scoped_map.of_litmus_id_map
        in
        State.make ~vars ())

    let run_test () : (State.t * Subject.Test.t) Or_error.t =
      State.Monad.(
        run'
          ( prepare_fuzzer_state ()
          >>= fun () ->
          Int.run (Lazy.force test_subject)
            ~payload:(Lazy.force random_state) )
          (Lazy.force init_fuzzer_state))

    let%expect_test "test int store: programs" =
      let r =
        let open Or_error.Let_syntax in
        let%map state, test = run_test () in
        let vars = State.vars state in
        List.mapi test.programs ~f:(fun id p ->
            let fn = Subject.Program.to_function ~vars ~id p in
            Act_c_mini.(Reify.func (Named.name fn) (Named.value fn)))
      in
      Fmt.(
        pr "%a@."
          (result ~ok:(list Act_c_lang.Ast.External_decl.pp) ~error:Error.pp))
        r ;
      [%expect
        {|
      void P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
      } |}]

    let%expect_test "test int store: global variables" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Var.Record.is_global])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen1 gen2 x y)) |}]

    let%expect_test "test int store: variables with known values" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Var.Record.has_known_value])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen2)) |}]

    let%expect_test "test int store: variables with dependencies" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Var.Record.has_dependencies])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen2)) |}]
  end )
