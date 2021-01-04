(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_data = struct
  let cmpxchg : C4f_fir.Expression.t C4f_fir.Atomic_cmpxchg.t Lazy.t =
    lazy
      Fir.
        { Atomic_cmpxchg.obj= Address.of_variable_str_exn "gen1"
        ; expected=
            Accessor.construct Address.variable_ref
              (C4f_common.C_id.of_string "expected")
        ; desired= Expression.int_lit 54321
        ; strength= Strong
        ; succ= Seq_cst
        ; fail= Relaxed }

  let cmpxchg_payload : Src.Atomic_cmpxchg.Insert.Inner_payload.t Lazy.t =
    Lazy.map cmpxchg ~f:(fun cmpxchg ->
        Src.Atomic_cmpxchg.Insert.Inner_payload.
          { cmpxchg
          ; exp_val= C4f_fir.Constant.int 12345
          ; exp_var= C4f_common.C_id.of_string "expected"
          ; out_var= C4f_common.C_id.of_string "out" })
end

let%test_module "cmpxchg.make.int.succeed" =
  ( module struct
    let random_state : Src.Atomic_cmpxchg.Insert.Int_succeed.Payload.t Lazy.t
        =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.cmpxchg_payload in
        let%map where = Fuzz_test.Subject.Test_data.Path.insert_live in
        Fuzz.Payload_impl.Pathed.make to_insert ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_cmpxchg.Insert.Int_succeed.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
      void
      P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
      {
          int expected = 12345;
          bool out = true;
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          out =
          atomic_compare_exchange_strong_explicit(gen1, &expected, 54321,
                                                  memory_order_seq_cst,
                                                  memory_order_relaxed);
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
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
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
        gen1: atomic_int*, =?, @global, generated, [Dep;Write]
        gen2: atomic_int*, =-55, @global, generated, []
        gen3: int*, =1998, @global, generated, []
        gen4: int*, =-4, @global, generated, []
        x: atomic_int*, =27, @global, generated, []
        y: atomic_int*, =53, @global, generated, []
        0:expected: int, =12345, @P0, generated, [Dep]
        0:out: bool, =true, @P0, generated, []
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]

    (* TODO(@MattWindsor91): dedupe this with the above *)
    let payload_dead : Src.Atomic_cmpxchg.Insert.Int_succeed.Payload.t Lazy.t
        =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.cmpxchg_payload in
        let%map where = Fuzz_test.Subject.Test_data.Path.insert_dead in
        Fuzz.Payload_impl.Pathed.make to_insert ~where)

    let test_action_dead : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_cmpxchg.Insert.Int_succeed.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force payload_dead))

    let%expect_test "in dead-code" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action_dead
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
           atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
        {
            int expected = 12345;
            bool out = true;
            atomic_int r0 = 4004;
            int r1 = 8008;
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y)
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            else
            {
                out =
                atomic_compare_exchange_strong_explicit(gen1, &expected, 54321,
                                                        memory_order_seq_cst,
                                                        memory_order_relaxed);
            }
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
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
           atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
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
          gen1: atomic_int*, =?, @global, generated, [Write]
          gen2: atomic_int*, =-55, @global, generated, []
          gen3: int*, =1998, @global, generated, []
          gen4: int*, =-4, @global, generated, []
          x: atomic_int*, =27, @global, generated, []
          y: atomic_int*, =53, @global, generated, []
          0:expected: int, =12345, @P0, generated, []
          0:out: bool, =true, @P0, generated, []
          0:r0: atomic_int, =4004, @P0, generated, []
          0:r1: int, =8008, @P0, generated, []
          1:r0: bool, =?, @P1, existing, []
          1:r1: int, =?, @P1, existing, []
          2:r0: int, =?, @P2, existing, []
          2:r1: bool, =?, @P2, existing, []
          3:r0: int*, =?, @P3, existing, [] |}]
  end )
