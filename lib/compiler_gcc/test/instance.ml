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

let%test_module "info_of_probe" =
  ( module struct
    let test (input : string list) : unit =
      Fmt.(
        pr "@[%a@]@." (result ~error:Error.pp ~ok:Act_compiler.Probe_info.pp))
        (Src.Instance.info_of_probe input)

    let%expect_test "valid x86_64 triplet" =
      test
        [ "Apple clang version 11.0.0 (clang-1100.0.33.17\n\
           Target: x86_64-apple-darwin19.2.0\n\
           Thread model: posix\n\
           InstalledDir: /blah"
        ; "4.9.0"
        ; "x86_64-apple-darwin" ] ;
      [%expect
        {|
        emits: x86.64
        version: 4.9.0
        name: AppleClang |}]

    let%expect_test "valid ppc64 triplet" =
      test
        [ "gcc-7 (Ubuntu 7.4.0-1ubuntu1~18.04.1) 7.4.0\nblah\nblah"
        ; "7.4.0"
        ; "powerpc64le-linux-gnu" ] ;
      [%expect
        {|
        emits: ppc.64.le
        version: 7.4.0
        name: gcc |}]

    let%expect_test "malformed (empty version string)" =
      test [""; "4.9.2"; "i686-linux-gnu"] ;
      [%expect {| no output from --version |}]

    let%expect_test "malformed (empty triplet string)" =
      test ["gcc 4.9.2"; "4.9.2"; ""] ;
      [%expect {| malformed target triplet: |}]
  end )
