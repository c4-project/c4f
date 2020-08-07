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
  let fadd : Act_fir.Expression.t Act_fir.Atomic_fetch.t Lazy.t =
    lazy
      Act_fir.(
        Atomic_fetch.make
          ~obj:(Address.of_variable_str_exn "gen1")
          ~arg:(Expression.int_lit 54321)
          ~mo:Seq_cst ~op:Add)

  let fadd_redundant : Act_fir.Expression.t Act_fir.Atomic_fetch.t Lazy.t =
    lazy
      Act_fir.(
        Atomic_fetch.make
          ~obj:(Address.of_variable_str_exn "gen1")
          ~arg:(Expression.int_lit 0) ~mo:Seq_cst ~op:Add)
end

let%test_module "fetch.make.int.dead" =
  ( module struct
    let path : F.Path.Flagged.t Lazy.t =
      FT.Subject.Test_data.Path.insert_dead

    let random_state : Src.Atomic_fetch.Insert.Int_dead.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.fadd in
        let%map where = path in
        F.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action : F.Subject.Test.t F.State.Monad.t =
      F.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_fetch.Insert.Int_dead.run
          (Lazy.force FT.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int fetch: programs" =
      FT.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          else { atomic_fetch_add_explicit(gen1, 54321, memory_order_seq_cst); }
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

    let%expect_test "test int fetch: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 x= y= |}]

    let%expect_test "test int fetch: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 r0=4004 |}]

    let%expect_test "test int fetch: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| |}]
  end )

let%test_module "fetch.make.int.redundant" =
  ( module struct
    let path : F.Path.Flagged.t Lazy.t =
      FT.Subject.Test_data.Path.insert_live

    let random_state : Src.Atomic_fetch.Insert.Int_redundant.Payload.t Lazy.t
        =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.fadd_redundant in
        let%map where = path in
        F.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action : F.Subject.Test.t F.State.Monad.t =
      F.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_fetch.Insert.Int_redundant.run
          (Lazy.force FT.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int fetch: programs" =
      FT.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_fetch_add_explicit(gen1, 0, memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int fetch: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 x= y= |}]

    let%expect_test "test int fetch: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 r0=4004 |}]

    let%expect_test "test int fetch: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| |}]
  end )
