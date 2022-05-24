(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

let%test_module "Early_out" =
  ( module struct
    let action_on_example_program (wherez : Fuzz.Path.With_meta.t Lazy.t)
        (kind : Fir.Early_out.t) : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      let where = Lazy.force wherez in
      let test : Fuzz.Subject.Test.t =
        Lazy.force Fuzz_test.Subject.Test_data.test
      in
      let payload =
        Fuzz.Payload_impl.Pathed.make ~where
          {Src.Flow_dead.Insert.Early_out_payload.if_cond= None; kind}
      in
      Src.Flow_dead.Insert.Early_out.run test ~payload

    let test_on_example_program (wherez : Fuzz.Path.With_meta.t Lazy.t)
        (kind : Fir.Early_out.t) : unit =
      let initial_state : Fuzz.State.t =
        Lazy.force Fuzz_test.State.Test_data.state
      in
      Fuzz_test.Action.Test_utils.run_and_dump_test
        (action_on_example_program wherez kind)
        ~initial_state

    (* TODO(@MattWindsor91): invalid paths *)

    let%expect_test "valid break on example program" =
      test_on_example_program
        Fuzz_test.Subject.Test_data.Path.insert_dead_loop Break ;
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
          while (4 == 5)
          { break; atomic_store_explicit(x, 44, memory_order_seq_cst); }
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
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "invalid break on example program" =
      test_on_example_program Fuzz_test.Subject.Test_data.Path.insert_dead
        Break ;
      [%expect
        {|
      ("checking flags on insertion" "Unmet required flag condition: in-loop") |}]

    let%expect_test "valid return on example program" =
      test_on_example_program Fuzz_test.Subject.Test_data.Path.insert_dead
        Return ;
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
          else { return; }
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
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]
  end )

let%test_module "Early_out_loop_end" =
  ( module struct
    let action_on_example_program ?(if_cond : Fir.Expression.t option)
        (wherez : Fuzz.Path.With_meta.t Lazy.t) (kind : Fir.Early_out.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      let where = Lazy.force wherez in
      let test : Fuzz.Subject.Test.t =
        Lazy.force Fuzz_test.Subject.Test_data.test
      in
      let payload =
        Fuzz.Payload_impl.Pathed.make ~where
          {Src.Flow_dead.Insert.Early_out_payload.if_cond; kind}
      in
      Src.Flow_dead.Insert.Early_out_loop_end.run test ~payload

    let test_on_example_program ?(if_cond : Fir.Expression.t option)
        (wherez : Fuzz.Path.With_meta.t Lazy.t) (kind : Fir.Early_out.t) :
        unit =
      let initial_state : Fuzz.State.t =
        Lazy.force Fuzz_test.State.Test_data.state
      in
      Fuzz_test.Action.Test_utils.run_and_dump_test
        (action_on_example_program ?if_cond wherez kind)
        ~initial_state

    let%expect_test "valid continue on multi loop" =
      test_on_example_program
        Fuzz_test.Subject.Test_data.Path.insert_multi_loop_end Continue ;
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
              { atomic_store_explicit(x, 99, memory_order_seq_cst); continue; }
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
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "invalid break on multi loop" =
      test_on_example_program
        Fuzz_test.Subject.Test_data.Path.insert_multi_loop_end Break ;
      [%expect {| "Unmet forbidden flag condition: in-execute-multi" |}]

    let%expect_test "valid continue on single loop" =
      test_on_example_program
        Fuzz_test.Subject.Test_data.Path.insert_once_loop_end Continue ;
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
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); continue; }
              while (4 == 5);
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
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "valid break on single loop" =
      test_on_example_program
        Fuzz_test.Subject.Test_data.Path.insert_once_loop_end Break ;
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
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); break; } while
              (4 == 5);
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
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "valid break on single loop with wrapping" =
      test_on_example_program ~if_cond:Fir.Expression.truth
        Fuzz_test.Subject.Test_data.Path.insert_once_loop_end Break ;
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
              do
              {
                  atomic_store_explicit(x, 44, memory_order_seq_cst);
                  if (true) { break; }
              } while (4 == 5);
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
            0:r0: atomic_int, =4004, @P0, generated, []
            0:r1: int, =8008, @P0, generated, []
            1:r0: bool, =?, @P1, existing, []
            1:r1: int, =?, @P1, existing, []
            2:r0: int, =?, @P2, existing, []
            2:r1: bool, =?, @P2, existing, []
            3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "wrapping introduces dependencies" =
      (* A fun fuzzer bug occurred when the wrapper introduced a very long
         and complex expression involving a variable that, owing to not being
         dependency-marked, was then used as the basis of a compare-exchange
         in another thread. It took hours to debug, and so this test tries to
         avoid the problem happening again. *)
      let wherez = Fuzz_test.Subject.Test_data.Path.insert_once_loop_end in
      test_on_example_program
        ~if_cond:
          Fir.Expression.(Infix.(of_variable_str_exn "r0" == int_lit 4004))
        wherez Break ;
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
            do
            {
                atomic_store_explicit(x, 44, memory_order_seq_cst);
                if (r0 == 4004) { break; }
            } while (4 == 5);
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
          0:r0: atomic_int, =4004, @P0, generated, [Dep]
          0:r1: int, =8008, @P0, generated, []
          1:r0: bool, =?, @P1, existing, []
          1:r1: int, =?, @P1, existing, []
          2:r0: int, =?, @P2, existing, []
          2:r1: bool, =?, @P2, existing, []
          3:r0: int*, =?, @P3, existing, [] |}]
  end )
