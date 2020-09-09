(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open struct
  module Src = Act_fuzz_actions
  module F = Act_fuzz
  module FT = Act_fuzz_test
end

let%test_module "Early_out" =
  ( module struct
    let test_on_example_program (wherez : F.Path.Flagged.t Lazy.t)
        (to_insert : Act_fir.Early_out.t) : unit =
      let where = Lazy.force wherez in
      let initial_state : F.State.t =
        Lazy.force FT.Subject.Test_data.state
      in
      let test : F.Subject.Test.t = Lazy.force FT.Subject.Test_data.test in
      let payload = F.Payload_impl.Insertion.make ~where ~to_insert in
      FT.Action.Test_utils.run_and_dump_test
        (Src.Flow_dead.Insert.Early_out.run test ~payload)
        ~initial_state

    (* TODO(@MattWindsor91): invalid paths *)

    let%expect_test "valid break on example program" =
      test_on_example_program FT.Subject.Test_data.Path.insert_dead_loop
        Break ;
      [%expect
        {|
      void
      P0(atomic_int *x, atomic_int *y)
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
          do { break; atomic_store_explicit(x, 44, memory_order_seq_cst); } while
          (4 == 5);
      }

      void
      P1(atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "invalid break on example program" =
      test_on_example_program FT.Subject.Test_data.Path.insert_dead Break ;
      [%expect
        {|
      ("checking flags on insertion" "Unmet required flag condition: in loop") |}]

    let%expect_test "valid return on example program" =
      test_on_example_program FT.Subject.Test_data.Path.insert_dead Return ;
      [%expect
        {|
      void
      P0(atomic_int *x, atomic_int *y)
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
      }

      void
      P1(atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
