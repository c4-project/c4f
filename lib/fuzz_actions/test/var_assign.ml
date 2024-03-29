(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_data = struct
  let store (name : string) : Fir.Assign.t Lazy.t =
    lazy
      Fir.(
        Assign.(
          Lvalue.of_variable_str_exn name
          @= Expression.atomic_load
               (Atomic_load.make
                  ~src:(Address.of_variable_str_exn "gen2")
                  ~mo:Mem_order.Seq_cst ) ) )
end

let%test_module "assign.insert.int.normal" =
  ( module struct
    let path : Fuzz.Path.With_meta.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    let random_state (store : Fir.Assign.t Lazy.t) :
        Src.Var_assign.Insert.Int_normal.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = store in
        let%map where = path in
        Fuzz.Payload_impl.Pathed.make to_insert ~where )

    let test_action (store : Fir.Assign.t Lazy.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Storelike.Test_common.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Var_assign.Insert.Int_normal.run
          (Lazy.force Fuzz_test.Subject.Test_data.test)
          ~payload:(Lazy.force (random_state store)) )

    let%test_module "store of load to global" =
      ( module struct
        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          test_action (Test_data.store "gen3")

        let%expect_test "test int store: programs" =
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
             gen3 = atomic_load_explicit(gen2, memory_order_seq_cst);
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
           gen1: atomic_int*, =1337, @global, generated, []
           gen2: atomic_int*, =-55, @global, generated, [Dep]
           gen3: int*, =?, @global, generated, [Write]
           gen4: int*, =-4, @global, generated, []
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

    let%test_module "store of load to local" =
      ( module struct
        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          test_action (Test_data.store "r1")

        let%expect_test "test int store: programs" =
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
             r1 = atomic_load_explicit(gen2, memory_order_seq_cst);
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
           gen1: atomic_int*, =1337, @global, generated, []
           gen2: atomic_int*, =-55, @global, generated, [Dep]
           gen3: int*, =1998, @global, generated, []
           gen4: int*, =-4, @global, generated, []
           x: atomic_int*, =27, @global, generated, []
           y: atomic_int*, =53, @global, generated, []
           0:r0: atomic_int, =4004, @P0, generated, []
           0:r1: int, =?, @P0, generated, [Write]
           1:r0: bool, =?, @P1, existing, []
           1:r1: int, =?, @P1, existing, []
           2:r0: int, =?, @P2, existing, []
           2:r1: bool, =?, @P2, existing, []
           3:r0: int*, =?, @P3, existing, [] |}]
      end )

    let%test_module "store.make.int.dead" =
      ( module struct
        let path : Fuzz.Path.With_meta.t Lazy.t =
          Fuzz_test.Subject.Test_data.Path.insert_dead

        let random_state : Src.Var_assign.Insert.Int_dead.Payload.t Lazy.t =
          Lazy.Let_syntax.(
            let%bind to_insert = Test_data.store "gen3" in
            let%map where = path in
            Fuzz.Payload_impl.Pathed.make to_insert ~where )

        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          Fuzz.State.Monad.(
            Storelike.Test_common.prepare_fuzzer_state ()
            >>= fun () ->
            Src.Var_assign.Insert.Int_dead.run
              (Lazy.force Fuzz_test.Subject.Test_data.test)
              ~payload:(Lazy.force random_state) )

        let%expect_test "test int store: programs" =
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
             else { gen3 = atomic_load_explicit(gen2, memory_order_seq_cst); }
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
           gen1: atomic_int*, =1337, @global, generated, []
           gen2: atomic_int*, =-55, @global, generated, []
           gen3: int*, =1998, @global, generated, [Write]
           gen4: int*, =-4, @global, generated, []
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

    let%test_module "store.make.int.redundant" =
      ( module struct
        let path : Fuzz.Path.With_meta.t Lazy.t =
          Fuzz_test.Subject.Test_data.Path.insert_live

        (* TODO(@MattWindsor91): this is, ironically, overly redundant. *)
        let redundant_store : Fir.Assign.t Lazy.t =
          lazy
            Fir.(
              Assign.(
                Lvalue.of_variable_str_exn "gen3"
                @= Fir.Expression.int_lit 1998 ) )

        let random_state :
            Src.Var_assign.Insert.Int_redundant.Payload.t Lazy.t =
          Lazy.Let_syntax.(
            let%bind to_insert = redundant_store in
            let%map where = path in
            Fuzz.Payload_impl.Pathed.make to_insert ~where )

        let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
          Fuzz.State.Monad.(
            Storelike.Test_common.prepare_fuzzer_state ()
            >>= fun () ->
            Src.Var_assign.Insert.Int_redundant.run
              (Lazy.force Fuzz_test.Subject.Test_data.test)
              ~payload:(Lazy.force random_state) )

        let%expect_test "test int store: programs" =
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
             gen3 = 1998;
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
           gen1: atomic_int*, =1337, @global, generated, []
           gen2: atomic_int*, =-55, @global, generated, []
           gen3: int*, =1998, @global, generated, [Write]
           gen4: int*, =-4, @global, generated, []
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
