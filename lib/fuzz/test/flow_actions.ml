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

let%test_module "If" =
  ( module struct
    module If = Src.Flow_actions.If

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : Src.State.t =
      Src.State.make ~vars:(Lazy.force Var.Test_data.test_map) ()

    let test : Src.Subject.Test.t = Lazy.force Subject.Example.test

    let cond : Act_c_mini.Expression.t =
      Act_c_mini.(
        Expression.(
          l_and
            (eq (variable (Act_common.C_id.of_string "d")) (int_lit 27))
            (variable (Act_common.C_id.of_string "a"))))

    let path : Src.Path_shapes.program =
      In_func (0, In_stms (On_stm_range (0, 2)))

    let payload : If.Payload.t = If.Payload.make ~cond ~path

    let%test_module "Tautology" =
      ( module struct
        let run_test () : (Src.State.t * Src.Subject.Test.t) Or_error.t =
          Src.State.Monad.run' (If.Tautology.run test ~payload) state

        let%expect_test "resulting AST" =
          Action.Test_utils.run_and_dump_test
            (If.Tautology.run test ~payload)
            ~initial_state:state ;
          [%expect
            {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, atomic_int e, int f, int foo, atomic_bool foobar)
        {
            if ((d == 27) && a)
            { atomic_store_explicit(x, 42, memory_order_seq_cst); ; }
            atomic_store_explicit(y, foo, memory_order_relaxed);
        } |}]

        let%expect_test "dependencies after running" =
          let result =
            Or_error.(
              run_test () >>| fst
              >>| Src.State.vars_satisfying_all ~scope:(Local 0)
                    ~predicates:[Src.Var.Record.has_dependencies])
          in
          print_s [%sexp (result : Act_common.C_id.t list Or_error.t)] ;
          [%expect {| (Ok (a d)) |}]
      end )
  end )
