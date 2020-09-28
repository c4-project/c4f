(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
         ~ok:(using Set.to_list (list ~sep:sp Act_common.Litmus_id.pp))))
    result

let%test_module "program.label" =
  ( module struct
    let path : Fuzz.Path.Flagged.t Lazy.t =
      Fuzz_test.Subject.Test_data.Path.insert_live

    let random_state : Act_common.C_id.t Fuzz.Payload_impl.Pathed.t Lazy.t =
      Lazy.Let_syntax.(
        let%map where = path in
        Fuzz.Payload_impl.Pathed.make
          (Act_common.C_id.of_string "label_mclabelface")
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
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "labels afterwards" =
      run_and_dump_labels test_action
        ~initial_state:(Lazy.force Fuzz_test.State.Test_data.state) ;
      [%expect {| 0:label_mclabelface |}]
  end )
