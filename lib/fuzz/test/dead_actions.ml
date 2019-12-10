(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Src = Act_fuzz

let%test_module "Early_out" =
  ( module struct
    let test_on_example_program (path : Src.Path.Program.t)
        (kind : Act_c_mini.Early_out.Kind.t) : unit =
      let initial_state : Src.State.t = Lazy.force Subject.Test_data.state in
      let test : Src.Subject.Test.t = Lazy.force Subject.Test_data.test in
      let payload = Src.Dead_actions.Early_out_payload.make ~path ~kind in
      Action.Test_utils.run_and_dump_test
        (Src.Dead_actions.Early_out.run test ~payload)
        ~initial_state

    let if_path : Src.Path.Program.t =
      Src.Path.(
        Program.in_thread 0 @@ Thread.in_stms @@ Stms.in_stm 3 @@ Stm.in_if
        @@ If.in_branch false @@ Stms.insert 0)

    let loop_path : Src.Path.Program.t =
      Src.Path.(
        Program.in_thread 0 @@ Thread.in_stms @@ Stms.in_stm 5 @@ Stm.in_loop
        @@ Loop.in_body @@ Stms.insert 0)

    (* TODO(@MattWindsor91): invalid paths *)

    let%expect_test "valid break on example program" =
      test_on_example_program loop_path Break ;
      [%expect
        {|
      void P0(atomic_int *x, atomic_int *y)
      {
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { break; atomic_store_explicit(x, 44, memory_order_seq_cst); } while
          (4 == 5);
      } |}]

    let%expect_test "invalid break on example program" =
      test_on_example_program if_path Break ;
      [%expect {|
      "Unmet flag condition: in loop" |}]

    let%expect_test "valid return on example program" =
      test_on_example_program if_path Return ;
      [%expect
        {|
      void P0(atomic_int *x, atomic_int *y)
      {
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
          else { return ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      } |}]
  end )
