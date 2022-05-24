(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let run_and_dump_labels
    (test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t)
    ~(initial_state : Fuzz.State.t) : unit =
  let result =
    Or_error.Let_syntax.(
      let%map {labels; _}, _ =
        Fuzz.State.Monad.(run' test_action initial_state)
      in
      labels)
  in
  Fmt.(
    pr "@[%a@]@."
      (result ~error:Error.pp
         ~ok:(using Set.to_list (list ~sep:sp C4f_common.Litmus_id.pp)) ))
    result

let%test_module "program.label" =
  ( module struct
    let path : Fuzz.Path.With_meta.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    let random_state : C4f_common.C_id.t Fuzz.Payload_impl.Pathed.t Lazy.t =
      Lazy.Let_syntax.(
        let%map where = path in
        Fuzz.Payload_impl.Pathed.make
          (C4f_common.C_id.of_string "label_mclabelface")
          ~where)

    let test_action : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Src.Program.Label.run
        (Lazy.force Fuzz_test.Subject.Test_data.test)
        ~payload:(Lazy.force random_state)

    let%expect_test "programs" =
      Fuzz_test.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
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
          label_mclabelface: ;
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
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]

    let%expect_test "labels afterwards" =
      run_and_dump_labels test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect {| 0:label_mclabelface |}]
  end )
