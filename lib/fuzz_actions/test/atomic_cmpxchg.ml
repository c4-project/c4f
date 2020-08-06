(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

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

module Test_data = struct
  let cmpxchg : Act_fir.Expression.t Act_fir.Atomic_cmpxchg.t Lazy.t =
    lazy
      Act_fir.(
        Atomic_cmpxchg.make
          ~obj:(Address.of_variable_str_exn "gen1")
          ~expected:
            (Address.of_variable_ref (Act_common.C_id.of_string "expected"))
          ~desired:(Expression.int_lit 54321)
          ~succ:Seq_cst ~fail:Relaxed)

  let cmpxchg_payload : Src.Atomic_cmpxchg.Inner_payload.t Lazy.t =
    Lazy.map cmpxchg ~f:(fun cmpxchg ->
        Src.Atomic_cmpxchg.Inner_payload.
          { cmpxchg
          ; exp_val= Act_fir.Constant.int 12345
          ; exp_var= Act_common.Litmus_id.of_string "0:expected"
          ; out_var= Act_common.Litmus_id.of_string "0:out" })
end

let%test_module "cmpxchg.make.int.always-succeed" =
  ( module struct
    let path : F.Path.Test.t Lazy.t = FT.Subject.Test_data.Path.insert_dead

    let random_state :
        Src.Atomic_cmpxchg.Insert_int_always_succeed.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.cmpxchg_payload in
        let%map where = path in
        F.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action : F.Subject.Test.t F.State.Monad.t =
      F.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_cmpxchg.Insert_int_always_succeed.run
          (Lazy.force FT.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "programs" =
      FT.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          int expected = 12345;
          bool out = true;
          atomic_int r0 = 4004;
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
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| gen1= gen2=-55 x= y= |}]

    let%expect_test "variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| expected=12345 gen2=-55 out=true r0=4004 |}]

    let%expect_test "variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| expected=12345 gen1= |}]
  end )
