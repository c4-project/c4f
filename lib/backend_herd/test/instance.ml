(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

open struct
  module Src = Act_backend_herd
  module Bk = Act_backend
  module Ac = Act_common
end

let%test_module "run" =
  ( module struct
    let spec : Bk.Spec.t =
      Bk.Spec.make ~cmd:"herd7" ~c_model:"c_lahav.cat"
        ~style:(Ac.Id.of_string "herd") ()

    let test_inner (arch : Bk.Arch.t) : string list Or_error.t =
      match Src.Instance.run spec ~arch with
      | Bk.Capability.Run.Cannot_run {why} ->
          Result.Error why
      | Can_run {argv_f} ->
          argv_f ~input_file:"example.litmus"

    let test (arch : Bk.Arch.t) : unit =
      print_s [%sexp (test_inner arch : string list Or_error.t)]

    let%expect_test "C" = test Bk.Arch.c ; [%expect]

    let%expect_test "Assembly (X86)" =
      test Bk.Arch.asm_x86 ; [%expect {| (-model c11_lahav.cat herd7) |}]
  end )
