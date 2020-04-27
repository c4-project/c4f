(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_c_mini
module Ac = Act_common

let in_env env x = Map.mem env (Src.Atomic_load.variable_of x)

let%expect_test "type_of: atomic_int* -> int" =
  let env = Lazy.force Env.test_env in
  let module Ty = Src.Atomic_load.Type_check (struct
    let env = env
  end) in
  let src =
    Src.Address.lvalue (Src.Lvalue.variable (Ac.C_id.of_string "bar"))
  in
  let ld = Src.Atomic_load.make ~src ~mo:Src.Mem_order.Seq_cst in
  print_s [%sexp (Ty.type_of ld : Src.Type.t Or_error.t)] ;
  [%expect {| (Ok int) |}]

let%test_unit "Quickcheck_atomic_ints: liveness" =
  let env = Lazy.force Env.test_env in
  let module Q = Src.Atomic_load.Quickcheck_atomic_ints (struct
    let env = env
  end) in
  Core_kernel.Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: Src.Atomic_load.t] ~f:(in_env env)

let%test_unit "Quickcheck_atomic_ints: generated underlying variables in \
               environment" =
  let env = Lazy.force Env.test_env in
  let module Q = Src.Atomic_load.Quickcheck_atomic_ints (struct
    let env = env
  end) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: Src.Atomic_load.t] ~here:[[%here]] (in_env env))
