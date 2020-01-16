(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_compiler_gcc
end

let%test_module "emits_of_probe" =
  ( module struct
    let test (input : string) : unit =
      Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:Act_common.Id.pp))
        (Src.Instance.emits_of_probe input)

    let%expect_test "valid x86_64 triplet" =
      test "x86_64-apple-darwin" ;
      [%expect {| x86.64 |}]

    let%expect_test "valid ppc64 triplet" =
      test "powerpc64le-linux-gnu" ;
      [%expect {| ppc.64.le |}]

    let%expect_test "malformed triplet (empty string)" =
      test "" ; [%expect {| malformed target triplet: |}]
  end )
