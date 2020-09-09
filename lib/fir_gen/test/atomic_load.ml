(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let in_env env x = Map.mem env (Fir.Atomic_load.variable_of x)

let%test_unit "Int: liveness" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Chk = Src.Atomic_load.Int (struct
    let env = env
  end) in
  Core_kernel.Quickcheck.test_can_generate [%quickcheck.generator: Chk.t]
    ~sexp_of:[%sexp_of: Fir.Atomic_load.t] ~f:(in_env env)

let%test_unit "Int: generated underlying variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Chk = Src.Atomic_load.Int (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Atomic_load.t] ~here:[[%here]] (in_env env))
