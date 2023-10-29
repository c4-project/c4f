(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "test runs" =
  ( module struct
    (* TODO(@MattWindsor91): sort out the discrepancy between the subject
       example and var map. Also sort out duplication with below. *)

    let state : Fuzz.State.t = Lazy.force Fuzz_test.State.Test_data.state

    let test : Fuzz.Subject.Test.t =
      Lazy.force Fuzz_test.Subject.Test_data.test

    let counter : Src.Flow_for.Payload.Counter.t =
      {var= C4f_common.Litmus_id.of_string "0:i"; ty= Fir.Type.int ()}

    let%test_module "Insert.kv_never" =
      ( module struct
        module Ins = Src.Flow_for.Insert.Kv_never

        let payload : Src.Flow_for.Payload.Kv.t Fuzz.Payload_impl.Pathed.t =
          { Fuzz.Payload_impl.Pathed.payload=
              { Src.Flow_for.Payload.Kv.lc= counter
              ; kv_val= Fir.Constant.int 27
              ; kv_expr=
                  Fir.(
                    Expression.atomic_load
                      (Atomic_load.make ~mo:Seq_cst
                         ~src:(Address.of_variable_str_exn "x") ) ) }
          ; where= Lazy.force Fuzz_test.Subject.Test_data.Path.insert_live }

        let action = Ins.run test ~payload

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
              int i = 0;
              atomic_int r0 = 4004;
              int r1 = 8008;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              ;
              for (i = 27; i > atomic_load_explicit(x, memory_order_seq_cst); i--) {  }
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
          { loop: ; if (true) {  } else { goto loop; } }

          Vars:
            a: bool, =false, @global, generated, []
            b: atomic_bool, =true, @global, generated, []
            bar: atomic_int, =?, @global, existing, []
            barbaz: bool, =?, @global, existing, []
            baz: atomic_int*, =?, @global, existing, []
            c: bool, =?, @global, generated, []
            d: int, =?, @global, existing, []
            e: int, =?, @global, generated, []
            foo: int, =?, @global, existing, []
            foobar: atomic_bool, =?, @global, existing, []
            x: atomic_int*, =27, @global, generated, [Dep]
            y: atomic_int*, =53, @global, generated, []
            0:i: int, =?, @P0, generated, [Dep]
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]
      end )

    let%test_module "Surround.kv_once" =
      ( module struct
        module Surround = Src.Flow_for.Surround.Kv_once

        let payload : Src.Flow_for.Payload.Kv.t Fuzz.Payload_impl.Pathed.t =
          Fuzz.Payload_impl.Pathed.make
            { Src.Flow_for.Payload.Kv.lc= counter
            ; kv_val= Fir.Constant.int 27
            ; kv_expr=
                Fir.(
                  Expression.atomic_load
                    (Atomic_load.make ~mo:Seq_cst
                       ~src:(Address.of_variable_str_exn "x") ) ) }
            ~where:
              (Lazy.force Fuzz_test.Subject.Test_data.Path.surround_atomic)

        let action = Surround.run test ~payload

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
              int i = 0;
              atomic_int r0 = 4004;
              int r1 = 8008;
              for (i = 27; i >= atomic_load_explicit(x, memory_order_seq_cst); i--)
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
              for (r1 = 0; r1 <= 2; r1++)
              { atomic_store_explicit(x, 99, memory_order_seq_cst); }
              while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } }

          Vars:
            a: bool, =false, @global, generated, []
            b: atomic_bool, =true, @global, generated, []
            bar: atomic_int, =?, @global, existing, []
            barbaz: bool, =?, @global, existing, []
            baz: atomic_int*, =?, @global, existing, []
            c: bool, =?, @global, generated, []
            d: int, =?, @global, existing, []
            e: int, =?, @global, generated, []
            foo: int, =?, @global, existing, []
            foobar: atomic_bool, =?, @global, existing, []
            x: atomic_int*, =27, @global, generated, [Dep]
            y: atomic_int*, =53, @global, generated, []
            0:i: int, =?, @P0, generated, [Dep]
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]
      end )

    let%test_module "Surround.Simple" =
      ( module struct
        module Surround = Src.Flow_for.Surround.Simple

        let payload :
            Src.Flow_for.Payload.Simple.t Fuzz.Payload_impl.Pathed.t =
          { payload= {lc= counter; up_to= Fir.Constant.int 53}
          ; where=
              Lazy.force Fuzz_test.Subject.Test_data.Path.surround_atomic }

        let action = Surround.run test ~payload

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
              int i = 0;
              atomic_int r0 = 4004;
              int r1 = 8008;
              for (i = 0; i <= 53; i++)
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
              for (r1 = 0; r1 <= 2; r1++)
              { atomic_store_explicit(x, 99, memory_order_seq_cst); }
              while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
          }

          void
          P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
             bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
             atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } }

          Vars:
            a: bool, =false, @global, generated, []
            b: atomic_bool, =true, @global, generated, []
            bar: atomic_int, =?, @global, existing, []
            barbaz: bool, =?, @global, existing, []
            baz: atomic_int*, =?, @global, existing, []
            c: bool, =?, @global, generated, []
            d: int, =?, @global, existing, []
            e: int, =?, @global, generated, []
            foo: int, =?, @global, existing, []
            foobar: atomic_bool, =?, @global, existing, []
            x: atomic_int*, =27, @global, generated, []
            y: atomic_int*, =53, @global, generated, []
            0:i: int, =?, @P0, generated, [Dep]
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]
      end )
  end )
