(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
open Import

open struct
  module Qx = Act_utils.My_quickcheck
end

let%test_module "Make" =
  ( module struct
    let%test_module "Payload" =
      ( module struct
        let printer
            ({basic_type; initial_value; var} : Src.Var.Make_payload.t) :
            unit =
          Fmt.pr "@[%s@ %a@ =@ %a;@]@."
            (Fir.Type.Basic.to_string basic_type)
            Common.Litmus_id.pp var Act_litmus_c.Reify_prim.pp_constant
            initial_value

        let make_gen (scope : Common.Scope.t) :
            (module Qx.S_sample with type t = Src.Var.Make_payload.t) =
          ( module struct
            include Src.Var.Make_payload

            let quickcheck_generator =
              Src.Var.Make_payload.generator
                (Lazy.force Fuzz_test.Var.Test_data.test_map)
                ~gen_scope:(Base_quickcheck.Generator.return scope)
          end )

        let make_gen' (scope : Common.Scope.t) :
            (module Test.S with type t = Src.Var.Make_payload.t) =
          (* TODO(@MattWindsor91): SURELY there's a better way of doing this *)
          ( module struct
            include (val make_gen scope)
          end )

        let print_sample (scope : Common.Scope.t) : unit =
          (* Expressions are quite big, so we tone down the generation
             parameters a bit. *)
          Qx.print_sample ~test_count:5 ~printer (make_gen scope)

        let%expect_test "global: samples" =
          print_sample Global ;
          [%expect
            {|
            atomic_int extravagant_cucumber_0 = -38250;
            atomic_int strong_zebra = -1;
            atomic_int intriguing_xylophone_0 = 1;
            atomic_int xenophilic_zodiac_3 = 470264907;
            atomic_int poor_yurt_1 = 2147483647; |}]

        let%expect_test "thread 0: samples" =
          print_sample (Local 0) ;
          [%expect
            {|
            atomic_bool 0:extravagant_staple_1 = false;
            atomic_bool 0:heap = true;
            atomic_bool 0:kelp_0 = true;
            int 0:vice_3 = -378;
            int 0:tall_house_1 = 1; |}]

        let test_type (scope : Common.Scope.t) : unit =
          Test.run_exn (make_gen' scope)
            ~f:(fun Src.Var.Make_payload.{basic_type; initial_value; _} ->
              let (_ : Act_fir.Type.t) =
                Or_error.ok_exn
                  Act_fir.(
                    Type.check_modulo_atomicity (Type.make basic_type)
                      (Constant.type_of initial_value))
              in
              ())

        let%test_unit "global: constant is the right type" = test_type Global

        let%test_unit "thread 0: constant is the right type" =
          test_type (Local 0)
      end )

    let%test_module "Example runs" =
      ( module struct
        let test_action (payload : Src.Var.Make_payload.t) :
            Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          Src.Var.Make.run
            (Lazy.force Fuzz_test.Subject.Test_data.test)
            ~payload

        let test (scope : Act_common.Scope.t) : unit =
          let pld =
            Src.Var.Make_payload.
              { basic_type= Act_fir.Type.Basic.int ()
              ; initial_value= Act_fir.Constant.int 42
              ; var=
                  Act_common.Litmus_id.make ~scope
                    ~id:(Act_common.C_id.of_string "foo") }
          in
          let action = test_action pld in
          Fuzz_test.Action.Test_utils.run_and_dump_test action
            ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state)

        let%expect_test "local" =
          test Act_common.Scope.(Local 1) ;
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
              ;
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
          { int foo = 42; loop: ; if (true) {  } else { goto loop; } }

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
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:foo: int, =42, @P1, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]

        let%expect_test "global" =
          test Act_common.Scope.Global ;
          [%expect
            {|
      void
      P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int *foo, atomic_bool foobar, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
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
         bool c, int d, int e, int *foo, atomic_bool foobar, atomic_int *x,
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
        foo: int*, =42, @global, generated, []
        foobar: atomic_bool, =?, @global, existing, []
        x: atomic_int*, =27, @global, generated, []
        y: atomic_int*, =53, @global, generated, []
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]
      end )
  end )

let%test_module "Volatile" =
  ( module struct
    let%test_module "Example runs" =
      ( module struct
        let test_action (payload : Act_common.Litmus_id.t) :
            Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          Src.Var.Volatile.run
            (Lazy.force Fuzz_test.Subject.Test_data.test)
            ~payload

        let test (tid : int) (name : string) : unit =
          let pld =
            Act_common.Litmus_id.local tid (Act_common.C_id.of_string name)
          in
          let action = test_action pld in
          Fuzz_test.Action.Test_utils.run_and_dump_test action
            ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state)

        let%expect_test "r0" =
          test 0 "r0" ;
          [%expect
            {|
            void
            P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
               bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
               atomic_int *y)
            {
                atomic_int volatile r0 = 4004;
                int r1 = 8008;
                atomic_store_explicit(x, 42, memory_order_seq_cst);
                ;
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
              0:r0: volatile atomic_int, =4004, @P0, generated, []
              0:r1: int, =8008, @P0, generated, []
              1:r0: bool, =?, @P1, existing, []
              1:r1: int, =?, @P1, existing, []
              2:r0: int, =?, @P2, existing, []
              2:r1: bool, =?, @P2, existing, []
              3:r0: int*, =?, @P3, existing, [] |}]
      end )
  end )
