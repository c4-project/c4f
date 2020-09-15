(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let in_env env x = Map.mem env x.@(Fir.Atomic_load.variable_of)

let print_sample =
  Utils.My_quickcheck.print_sample
    ~printer:
      Fmt.(
        pr "@[%a@]@."
          (using Fir.Expression.atomic_load Act_litmus_c.Reify_expr.pp))

let%expect_test "Int: samples" =
  let env = Lazy.force Fir_test.Env.test_env in
  print_sample
    ( module struct
      include Fir.Atomic_load

      include Src.Atomic_load.Int (struct
        let env = env
      end)
    end ) ;
  [%expect
    {|
    atomic_load_explicit(bar, memory_order_consume)
    atomic_load_explicit(bar, memory_order_acquire)
    atomic_load_explicit(bar, memory_order_seq_cst)
    atomic_load_explicit(&x, memory_order_relaxed)
    atomic_load_explicit(&x, memory_order_consume)
    atomic_load_explicit(&x, memory_order_acquire)
    atomic_load_explicit(&x, memory_order_seq_cst)
    atomic_load_explicit(&y, memory_order_relaxed)
    atomic_load_explicit(&y, memory_order_consume)
    atomic_load_explicit(&y, memory_order_acquire)
    atomic_load_explicit(&y, memory_order_seq_cst) |}]

let%test_unit "Int: generated underlying variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Chk = Src.Atomic_load.Int (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Atomic_load.t] ~here:[[%here]] (in_env env))
