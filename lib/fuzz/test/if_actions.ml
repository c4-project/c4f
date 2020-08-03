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
    module Surround = Src.If_actions.Surround

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : Src.State.t =
      Src.State.make ~vars:(Lazy.force Var.Test_data.test_map) ()

    let test : Src.Subject.Test.t = Lazy.force Subject.Test_data.test

    let cond : Act_fir.Expression.t =
      (* should be true with respect to the test var-map *)
      Act_fir.(
        Expression.(
          l_and
            (eq (of_variable_str_exn "x") (int_lit 27))
            (of_variable_str_exn "a")))

    let where : Src.Path.Test.t =
      Lazy.force Subject.Test_data.Path.surround_atomic

    let payload : Src.Payload.Cond_surround.t =
      Src.Payload.Cond_surround.make ~cond ~where

    let%test_module "Tautology" =
      ( module struct
        let action = Surround.Tautology.run test ~payload

        let%expect_test "resulting AST" =
          Action.Test_utils.run_and_dump_test action ~initial_state:state ;
          [%expect
            {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
           atomic_int y)
        {
            atomic_int r0 = 4004;
            if (x == 27 && a)
            { atomic_store_explicit(x, 42, memory_order_seq_cst); ; }
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y)
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            if (false)
            {
                atomic_store_explicit(y,
                                      atomic_load_explicit(x, memory_order_seq_cst),
                                      memory_order_seq_cst);
            }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        }

        void
        P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
           atomic_int y)
        { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "global dependencies after running" =
          Action.Test_utils.run_and_dump_global_deps action
            ~initial_state:state ;
          [%expect {|
          a
          x |}]
      end )

    let%test_module "Duplicate" =
      ( module struct
        let action = Surround.Duplicate.run test ~payload

        let%expect_test "resulting AST" =
          Action.Test_utils.run_and_dump_test action ~initial_state:state ;
          [%expect
            {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
           atomic_int y)
        {
            atomic_int r0 = 4004;
            if (x == 27 && a)
            { atomic_store_explicit(x, 42, memory_order_seq_cst); ; } else
            { atomic_store_explicit(x, 42, memory_order_seq_cst); ; }
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y)
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            if (false)
            {
                atomic_store_explicit(y,
                                      atomic_load_explicit(x, memory_order_seq_cst),
                                      memory_order_seq_cst);
            }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        }

        void
        P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
           atomic_int y)
        { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "global dependencies after running" =
          Action.Test_utils.run_and_dump_global_deps action
            ~initial_state:state ;
          [%expect {|
          a
          x |}]

        let label_direct : Src.Path.Test.t =
          Lazy.force Subject.Test_data.Path.surround_label_direct

        let label_direct_payload : Src.Payload.Cond_surround.t =
          Src.Payload.Cond_surround.make ~cond ~where:label_direct

        let%expect_test "direct-label AST (should fail)" =
          Action.Test_utils.run_and_dump_test
            (Surround.Duplicate.run test ~payload:label_direct_payload)
            ~initial_state:state ;
          [%expect
            {| ("Statement failed check" (check (Stm_class Is_not_any ((Prim (Label)))))) |}]

        let label_indirect : Src.Path.Test.t =
          Lazy.force Subject.Test_data.Path.surround_label_indirect

        let label_indirect_payload : Src.Payload.Cond_surround.t =
          Src.Payload.Cond_surround.make ~cond ~where:label_indirect

        let%expect_test "indirect-label AST (should fail)" =
          Action.Test_utils.run_and_dump_test
            (Surround.Duplicate.run test ~payload:label_indirect_payload)
            ~initial_state:state ;
          [%expect
            {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          {
              atomic_int r0 = 4004;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              ;
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (x == 27 && a)
              {
                  if (foo == y)
                  { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ;
                  }
              } else
              {
                  if (foo == y)
                  { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ;
                  }
              }
              if (false)
              {
                  atomic_store_explicit(y,
                                        atomic_load_explicit(x, memory_order_seq_cst),
                                        memory_order_seq_cst);
              }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
      end )
  end )

let%test_module "Invert" =
  ( module struct
    let initial_state : Src.State.t = Lazy.force Subject.Test_data.state

    let test : Src.Subject.Test.t = Lazy.force Subject.Test_data.test

    let payload : Src.Path.Test.t =
      Src.Path.(
        Test.in_thread 0 @@ Thread.in_stms @@ Stms.in_stm 3 @@ Stm.this_stm)

    let%expect_test "resulting AST" =
      Action.Test_utils.run_and_dump_test
        (Src.If_actions.Invert.run test ~payload)
        ~initial_state ;
      [%expect
        {|
        void
        P0(atomic_int *x, atomic_int *y)
        {
            atomic_int r0 = 4004;
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (!(foo == y)) {  } else
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            if (false)
            {
                atomic_store_explicit(y,
                                      atomic_load_explicit(x, memory_order_seq_cst),
                                      memory_order_seq_cst);
            }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        }

        void
        P1(atomic_int *x, atomic_int *y)
        { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
