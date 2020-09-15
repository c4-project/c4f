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
  let store (name : string) : Fir.Atomic_store.t Lazy.t =
    lazy
      Fir.(
        Atomic_store.make
          ~src:
            (Expression.atomic_load
               (Atomic_load.make
                  ~src:(Address.of_variable_str_exn "gen2")
                  ~mo:Mem_order.Seq_cst))
          ~dst:(Address.of_variable_str_exn name)
          ~mo:Mem_order.Seq_cst)

  let fadd : Fir.Expression.t Lazy.t =
    lazy
      Fir.(
        Expression.(
          atomic_fetch
            (Atomic_fetch.make
               ~obj:(Address.of_variable_str_exn "gen1")
               ~arg:(Expression.int_lit 0) ~mo:Seq_cst ~op:Add)))

  let store_fa : Fir.Atomic_store.t Lazy.t =
    lazy
      Fir.(
        Atomic_store.make
          ~src:
            Fir.(
              Expression.(
                atomic_fetch
                  (Atomic_fetch.make
                     ~obj:(Address.of_variable_str_exn "gen1")
                     ~arg:(sub (Lazy.force fadd) (Lazy.force fadd))
                     ~mo:Seq_cst ~op:Sub)))
          ~dst:(Address.of_variable_str_exn "gen1")
          ~mo:Seq_cst)
end

let%test_module "atomic.store.insert.int.normal" =
  ( module struct
    let path : Fuzz.Path.Flagged.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    let random_state (store : Fir.Atomic_store.t Lazy.t) :
        Src.Atomic_store.Insert.Int_normal.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = store in
        let%map where = path in
        Fuzz.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action (store : Fir.Atomic_store.t Lazy.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_store.Insert.Int_normal.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force (random_state store)))

    let%test_module "store of load to global" =
      ( module struct
        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          test_action (Test_data.store "gen1")

        let%expect_test "test int store: programs" =
          Fuzz_test.Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          Storelike.Test_common.run_and_dump_globals test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen1= gen2=-55 gen3=1998 gen4=-4 x= y= |}]

        let%expect_test "test int store: variables with known values" =
          Storelike.Test_common.run_and_dump_kvs test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 |}]

        let%expect_test "test int store: variables with dependencies" =
          Storelike.Test_common.run_and_dump_deps test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen2=-55 |}]
      end )

    let%test_module "store of load to local" =
      ( module struct
        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          test_action (Test_data.store "r0")

        let%expect_test "test int store: programs" =
          Fuzz_test.Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(r0,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          Storelike.Test_common.run_and_dump_globals test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 x= y= |}]

        let%expect_test "test int store: variables with known values" =
          Storelike.Test_common.run_and_dump_kvs test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 r1=8008 |}]

        let%expect_test "test int store: variables with dependencies" =
          Storelike.Test_common.run_and_dump_deps test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen2=-55 |}]
      end )

    let%test_module "store of self-referential fetching" =
      ( module struct
        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          test_action Test_data.store_fa

        let%expect_test "test int store: programs" =
          Fuzz_test.Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_fetch_sub_explicit(gen1,
                                                          atomic_fetch_add_explicit
                                                          (gen1, 0,
                                                           memory_order_seq_cst)
                                                          -
                                                          atomic_fetch_add_explicit
                                                          (gen1, 0,
                                                           memory_order_seq_cst),
                                                          memory_order_seq_cst),
                                memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          Storelike.Test_common.run_and_dump_globals test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen1= gen2=-55 gen3=1998 gen4=-4 x= y= |}]

        let%expect_test "test int store: variables with known values" =
          Storelike.Test_common.run_and_dump_kvs test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 |}]

        let%expect_test "test int store: variables with dependencies" =
          Storelike.Test_common.run_and_dump_deps test_action
            ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
          [%expect {| gen1= |}]
      end )
  end )

let%test_module "store.make.int.dead" =
  ( module struct
    let path : Fuzz.Path.Flagged.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_dead

    let random_state : Src.Atomic_store.Insert.Int_dead.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.store "gen1" in
        let%map where = path in
        Fuzz.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_store.Insert.Int_dead.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int store: programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          else
          {
              atomic_store_explicit(gen1,
                                    atomic_load_explicit(gen2,
                                                         memory_order_seq_cst),
                                    memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int store: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 x= y= |}]

    let%expect_test "test int store: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 |}]

    let%expect_test "test int store: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| |}]
  end )

let%test_module "store.make.int.redundant" =
  ( module struct
    let path : Fuzz.Path.Flagged.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    (* TODO(@MattWindsor91): this is, ironically, overly redundant. *)
    let redundant_store : Fir.Atomic_store.t Lazy.t =
      lazy
        Fir.(
          Atomic_store.make
            ~src:(Fir.Expression.int_lit 1337)
            ~dst:(Address.of_variable_str_exn "gen1")
            ~mo:Mem_order.Seq_cst)

    let random_state : Src.Atomic_store.Insert.Int_redundant.Payload.t Lazy.t
        =
      Lazy.Let_syntax.(
        let%bind to_insert = redundant_store in
        let%map where = path in
        Fuzz.Payload_impl.Insertion.make ~to_insert ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Atomic_store.Insert.Int_redundant.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int store: programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1, 1337, memory_order_seq_cst);
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
      P1(atomic_int *gen1, atomic_int *gen2, int *gen3, int *gen4, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int store: global variables" =
      Storelike.Test_common.run_and_dump_globals test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 x= y= |}]

    let%expect_test "test int store: variables with known values" =
      Storelike.Test_common.run_and_dump_kvs test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| gen1=1337 gen2=-55 gen3=1998 gen4=-4 r0=4004 r1=8008 |}]

    let%expect_test "test int store: variables with dependencies" =
      Storelike.Test_common.run_and_dump_deps test_action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state) ;
      [%expect {| |}]
  end )

let%test_module "xchgify" =
  ( module struct
    let test_action (payload : Fuzz.Path.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Src.Atomic_store.Transform.Xchgify.run
        (Lazy.force Fuzz_test.Subject.Test_data.test)
        ~payload

    let test (lpath : Fuzz.Path.t Lazy.t) : unit =
      let path = Lazy.force lpath in
      let action = test_action path in
      Fuzz_test.Action.Test_utils.run_and_dump_test action
        ~initial_state:(Lazy.force Fuzz_test.Subject.Test_data.state)

    let%expect_test "example store" =
      test
        Fuzz.Path.(
          Fuzz_test.Subject.Test_data.Path.thread_0_stms @@ Stms.in_stm 0
          @@ Stm.this_stm) ;
      [%expect
        {|
      void
      P0(atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
          atomic_exchange_explicit(x, 42, memory_order_seq_cst);
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
      }

      void
      P1(atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
