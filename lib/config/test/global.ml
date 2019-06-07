(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_config
module Ac = Act_common
module Am = Act_machine

let%test_module "accessors" =
  ( module struct
    let cpp : Cpp.t = Cpp.default ()

    let fuzz : Fuzz.t = Fuzz.make ()

    let defaults : Ast.Default.t list =
      Ast.Default.
        [ Try (Compiler, Ac.Id.of_string "localhost.gcc.x86.normal")
        ; Try (Arch, Ac.Id.of_string "x86.att") ]

    let machines : Am.Spec.Set.t =
      Lazy.force Act_machine_test.Data.Spec_sets.single_local_machine

    let global : Global.t = Global.make ~cpp ~fuzz ~defaults ~machines ()

    let%expect_test "cpp" =
      print_s [%sexp (Global.cpp global : Cpp.t option)] ;
      [%expect {| (((enabled true) (cmd ()) (argv ()))) |}]

    let%expect_test "defaults" =
      print_s [%sexp (Global.defaults global : Ast.Default.t list)] ;
      [%expect
        {| ((Try compiler (localhost gcc x86 normal)) (Try arch (x86 att))) |}]

    let%expect_test "fuzz" =
      print_s [%sexp (Global.fuzz global : Fuzz.t option)] ;
      [%expect {| (((weights ()))) |}]

    let%expect_test "machines" =
      Fmt.pr "@[%a@]@." Am.Spec.Set.pp (Global.machines global) ;
      [%expect {| localhost: local |}]
  end )
