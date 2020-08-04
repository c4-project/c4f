(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fuzz_actions
  module F = Act_fuzz
  module FT = Act_fuzz_test
end

let%test_module "Sync_surround" =
  ( module struct
    module Surround = Src.Lock.Sync_surround

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : F.State.t =
      F.State.make ~vars:(Lazy.force FT.Var.Test_data.test_map) ()

    let test : F.Subject.Test.t = Lazy.force FT.Subject.Test_data.test

    let action =
      Surround.run test
        ~payload:(Lazy.force FT.Subject.Test_data.Path.surround_atomic)

    let%expect_test "resulting AST" =
      FT.Action.Test_utils.run_and_dump_test action ~initial_state:state ;
      [%expect
        {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          {
              atomic_int r0 = 4004;
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
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
  end )

let%test_module "Atomic_surround" =
  ( module struct
    module Surround = Src.Lock.Atomic_surround

    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. *)

    let state : F.State.t =
      F.State.make ~vars:(Lazy.force FT.Var.Test_data.test_map) ()

    let test : F.Subject.Test.t = Lazy.force FT.Subject.Test_data.test

    let action =
      Surround.run test
        ~payload:(Lazy.force FT.Subject.Test_data.Path.surround_txsafe)

    let%expect_test "resulting AST" =
      FT.Action.Test_utils.run_and_dump_test action ~initial_state:state ;
      [%expect
        {|
          void
          P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          {
              atomic_int r0 = 4004;
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
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int x,
             atomic_int y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
