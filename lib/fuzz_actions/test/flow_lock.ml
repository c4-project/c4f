(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let lift (path : Fuzz.Path.Flagged.t Lazy.t) :
    unit Fuzz.Payload_impl.Pathed.t =
  Fuzz.Payload_impl.Pathed.make () ~where:(Lazy.force path)

let%test_module "Surround.Sync" =
  ( module struct
    module Surround = Src.Flow_lock.Surround.Sync

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : Fuzz.State.t = Lazy.force Fuzz_test.State.Test_data.state

    let test : Fuzz.Subject.Test.t =
      Lazy.force Fuzz_test.Subject.Test_data.test

    let action =
      Surround.run test
        ~payload:(lift Fuzz_test.Subject.Test_data.Path.surround_atomic)

    let%expect_test "resulting AST" =
      Fuzz_test.Action.Test_utils.run_and_dump_test action
        ~initial_state:state ;
      [%expect
        {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          {
              atomic_int r0 = 4004;
              int r1 = 8008;
              synchronized { atomic_store_explicit(x, 42, memory_order_seq_cst); ; }
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
              for (r1 = 0; r1 <= 2; r1++)
              { atomic_store_explicit(x, 99, memory_order_seq_cst); }
              while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
  end )

let%test_module "Surround.Atomic" =
  ( module struct
    module Surround = Src.Flow_lock.Surround.Atomic

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : Fuzz.State.t = Lazy.force Fuzz_test.State.Test_data.state

    let test : Fuzz.Subject.Test.t =
      Lazy.force Fuzz_test.Subject.Test_data.test

    let action =
      Surround.run test
        ~payload:(lift Fuzz_test.Subject.Test_data.Path.surround_txsafe)

    let%expect_test "resulting AST" =
      Fuzz_test.Action.Test_utils.run_and_dump_test action
        ~initial_state:state ;
      [%expect
        {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          {
              atomic_int r0 = 4004;
              int r1 = 8008;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              atomic { ; }
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
              for (r1 = 0; r1 <= 2; r1++)
              { atomic_store_explicit(x, 99, memory_order_seq_cst); }
              while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
