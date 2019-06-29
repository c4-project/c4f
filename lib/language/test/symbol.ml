(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_language.Symbol

let%expect_test "program_id_of_demangled: valid" =
  print_s [%sexp (program_id_of_demangled "P0" : int option)] ;
  [%expect {| (0) |}]
