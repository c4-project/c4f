(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_fuzz

let%test_module "int tests" =
  ( module struct
    let path : Src.Path.Program.t Lazy.t =
      lazy Src.Path.(Program.in_thread 0 @@ Thread.in_stms @@ Stms.insert 2)

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

    let random_state : Src.Store_actions.Int.Payload.t Lazy.t =
      let open Lazy.Let_syntax in
      let%bind store = store in
      let%map path = path in
      Src.Store_actions.Random_state.make ~store ~path

    let prepare_fuzzer_state () : unit Src.State.Monad.t =
      Src.State.Monad.(
        register_global
          Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
          (Act_common.C_id.of_string "gen1")
          ~initial_value:(Act_c_mini.Constant.int 1337)
        >>= fun () ->
        register_global
          Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
          (Act_common.C_id.of_string "gen2")
          ~initial_value:(Act_c_mini.Constant.int (-55)))

    let test_action : Src.Subject.Test.t Src.State.Monad.t =
      Src.State.Monad.(
        prepare_fuzzer_state ()
        >>= fun () ->
        Src.Store_actions.Int.run
          (Lazy.force Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let run_test () : (Src.State.t * Src.Subject.Test.t) Or_error.t =
      Src.State.Monad.(run' test_action (Lazy.force Subject.Test_data.state))

    let%expect_test "test int store: programs" =
      Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Subject.Test_data.state) ;
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
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
      } |}]

    let%expect_test "test int store: global variables" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| Src.State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Src.Var.Record.is_global])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen1 gen2 x y)) |}]

    let%expect_test "test int store: variables with known values" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| Src.State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Src.Var.Record.has_known_value])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen2)) |}]

    let%expect_test "test int store: variables with dependencies" =
      let result =
        Or_error.(
          run_test () >>| fst
          >>| Src.State.vars_satisfying_all ~scope:(Local 0)
                ~predicates:[Src.Var.Record.has_dependencies])
      in
      print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
      [%expect {| (Ok (gen2)) |}]
  end )
