(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_fir

let%expect_test "type_of: atomic_int* -> int" =
  let env = Lazy.force Env.test_env in
  let module Ty = Src.Atomic_load.Type_check (struct
    let env = env
  end) in
  let src = Src.Address.of_variable_str_exn "bar" in
  let ld = Src.Atomic_load.make ~src ~mo:Src.Mem_order.Seq_cst in
  print_s [%sexp (Ty.type_of ld : Src.Type.t Or_error.t)] ;
  [%expect {| (Ok int) |}]
