(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "expressions" =
  ( module struct
    let test (exp : Src.Expression.t) : unit =
      Src.Litmus_stats.(
        Utils.My_format.fdump Stdio.stdout Statset.pp (scrape_expr exp))

    let%expect_test "regression: ternaries" =
      test
        Src.Expression.(
          ternary
            { if_= truth
            ; then_= int_lit 27
            ; else_=
                atomic_load
                  (Src.Atomic_load.make
                     ~src:(Src.Address.of_variable_str_exn "foo")
                     ~mo:Seq_cst) }) ;
      [%expect
        {|
        threads 0
        returns 0
        literals.bool 1
        atomics.expression.cmpxchg 0
        atomics.expression.fence 0
        atomics.expression.fetch 0
        atomics.expression.load 1
        atomics.expression.store 0
        atomics.expression.xchg 0
        mem-orders.expression.memory_order_relaxed 0
        mem-orders.expression.memory_order_consume 0
        mem-orders.expression.memory_order_acquire 0
        mem-orders.expression.memory_order_release 0
        mem-orders.expression.memory_order_acq_rel 0
        mem-orders.expression.memory_order_seq_cst 1
        atomics.statement.cmpxchg 0
        atomics.statement.fence 0
        atomics.statement.fetch 0
        atomics.statement.load 0
        atomics.statement.store 0
        atomics.statement.xchg 0
        mem-orders.statement.memory_order_relaxed 0
        mem-orders.statement.memory_order_consume 0
        mem-orders.statement.memory_order_acquire 0
        mem-orders.statement.memory_order_release 0
        mem-orders.statement.memory_order_acq_rel 0
        mem-orders.statement.memory_order_seq_cst 0 |}]
  end )
