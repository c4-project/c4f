(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_fuzz

let%test_module "Surround" =
  ( module struct
    module Surround = Src.Loop_actions.Surround

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : Src.State.t =
      Src.State.make
        ~labels:(Set.empty (module Src.Label))
        ~vars:(Lazy.force Var.Test_data.test_map)
        ()

    let test : Src.Subject.Test.t = Lazy.force Subject.Test_data.test

    let cond : Act_c_mini.Expression.t =
      (* should be false with respect to the var map *)
      Act_c_mini.(
        Expression.(
          l_and
            (eq (of_variable_str_exn "y") (int_lit 27))
            (of_variable_str_exn "a")))

    let path : Src.Path.Program.t =
      Src.Path.(Program.in_thread 0 @@ Thread.in_stms @@ Stms.on_range 0 2)

    let payload : Src.Payload.Surround.t =
      Src.Payload.Surround.make ~cond ~path

    let action = Surround.run test ~payload

    let%expect_test "resulting AST" =
      Action.Test_utils.run_and_dump_test action ~initial_state:state ;
      [%expect
        {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          {
              do { atomic_store_explicit(x, 42, memory_order_seq_cst); ; } while (y ==
              27 && a);
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
              if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
          } |}]

    let%expect_test "global dependencies after running" =
      Action.Test_utils.run_and_dump_global_deps action ~initial_state:state ;
      [%expect {|
          a
          y |}]
  end )

let%test_module "Invert" =
  ( module struct
    let initial_state : Src.State.t = Lazy.force Subject.Test_data.state

    let test : Src.Subject.Test.t = Lazy.force Subject.Test_data.test

    let payload : Src.Path.Program.t =
      Src.Path.(
        Program.in_thread 0 @@ Thread.in_stms @@ Stms.in_stm 3
        @@ Stm.this_stm)

    let%expect_test "resulting AST" =
      Action.Test_utils.run_and_dump_test
        (Src.If_actions.Invert.run test ~payload)
        ~initial_state ;
      [%expect
        {|
        void P0(atomic_int *x, atomic_int *y)
        {
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (!(foo == y)) {  } else
            { atomic_store_explicit(x, 56, memory_order_seq_cst); }
            if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        } |}]
  end )
