(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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

let run_and_dump_labels (test_action : F.Subject.Test.t F.State.Monad.t)
    ~(initial_state : F.State.t) : unit =
  let result =
    Or_error.(
      F.State.Monad.(run' test_action initial_state)
      >>| fst >>| F.State.labels)
  in
  Fmt.(
    pr "@[%a@]@."
      (result ~error:Error.pp
         ~ok:(using Set.to_list (list ~sep:sp Act_common.Litmus_id.pp))))
    result

let%test_module "program.label" =
  ( module struct
    let path : F.Path.Flagged.t Lazy.t =
      FT.Subject.Test_data.Path.insert_live

    let random_state : Act_common.C_id.t F.Payload_impl.Insertion.t Lazy.t =
      Lazy.Let_syntax.(
        let%map where = path in
        F.Payload_impl.Insertion.make
          ~to_insert:(Act_common.C_id.of_string "label_mclabelface")
          ~where)

    let test_action : F.Subject.Test.t F.State.Monad.t =
      Src.Program.Label.run
        (Lazy.force FT.Subject.Test_data.test)
        ~payload:(Lazy.force random_state)

    let%expect_test "programs" =
      FT.Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
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
      }

      void
      P1(atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "labels afterwards" =
      run_and_dump_labels test_action
        ~initial_state:(Lazy.force FT.Subject.Test_data.state) ;
      [%expect {| 0:label_mclabelface |}]
  end )
