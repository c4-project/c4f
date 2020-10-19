(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_data = struct
  let fadd : Fir.Expression.t Fir.Atomic_fetch.t Lazy.t =
    lazy
      Fir.(
        Atomic_fetch.make
          ~obj:(Address.of_variable_str_exn "gen1")
          ~arg:(Expression.int_lit 54321)
          ~mo:Seq_cst ~op:Add)

  let fadd_redundant : Fir.Expression.t Fir.Atomic_fetch.t Lazy.t =
    lazy
      Fir.(
        Atomic_fetch.make
          ~obj:(Address.of_variable_str_exn "gen1")
          ~arg:(Expression.int_lit 0) ~mo:Seq_cst ~op:Add)
end

let%test_module "fetch.make.int.dead" =
  ( module struct
    let path : Fuzz.Path.With_meta.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_dead

    let random_state : Src.Atomic_fetch.Insert.Int_dead.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.fadd in
        let%map where = path in
        Fuzz.Payload_impl.Pathed.make to_insert ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_fetch.Insert.Int_dead.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int fetch: programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
      void
      P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
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
          for (r1 = 0; r1 <= 2; r1++)
          { atomic_store_explicit(x, 99, memory_order_seq_cst); }
          while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
      }

      void
      P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int fetch: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
        a=false b=true bar= barbaz= baz= c= d= e= foo= foobar= gen1=1337 gen2=-55
        gen3=1998 gen4=-4 x=27 y=53 |}]

    let%expect_test "test int fetch: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {| a=false b=true gen1=1337 gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 x=27 y=53 |}]

    let%expect_test "test int fetch: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect {| |}]
  end )

let%test_module "fetch.make.int.redundant" =
  ( module struct
    let path : Fuzz.Path.With_meta.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    let random_state : Src.Atomic_fetch.Insert.Int_redundant.Payload.t Lazy.t
        =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.fadd_redundant in
        let%map where = path in
        Fuzz.Payload_impl.Pathed.make to_insert ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_fetch.Insert.Int_redundant.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int fetch: programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
      void
      P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
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
          for (r1 = 0; r1 <= 2; r1++)
          { atomic_store_explicit(x, 99, memory_order_seq_cst); }
          while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
      }

      void
      P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *gen1,
         atomic_int *gen2, int *gen3, int *gen4, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int fetch: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {|
        a=false b=true bar= barbaz= baz= c= d= e= foo= foobar= gen1=1337 gen2=-55
        gen3=1998 gen4=-4 x=27 y=53 |}]

    let%expect_test "test int fetch: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect
        {| a=false b=true gen1=1337 gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 x=27 y=53 |}]

    let%expect_test "test int fetch: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect {| |}]
  end )
