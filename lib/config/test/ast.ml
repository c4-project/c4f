(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Id = Act_common.Id
module Ast = Act_config.Ast

let%test_module "pretty-printing" =
  ( module struct
    let test (pp : 'a Fmt.t) (value : 'a) : unit =
      Fmt.pr "@[%a@]@." pp value

    let%expect_test "example compiler" =
      test Ast.Machine.pp
        (Ast.Machine.Compiler
           ( Id.of_string "foo.bar.baz"
           , Ast.Compiler.
               [ Enabled true
               ; Style (Id.of_string "gcc")
               ; Emits (Id.of_string "att.x86")
               ; Cmd "gcc"
               ; Argv ["-m32"; "-fno-pic"] ] )) ;
      [%expect
        {|
      compiler foo.bar.baz {
        enabled yes
        style gcc
        arch att.x86
        cmd "gcc"
        argv "-m32" "-fno-pic"
      } |}]
  end )
