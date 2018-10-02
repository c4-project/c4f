(*
This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor
   (parts (c) 2010-2018 Institut National de Recherche en Informatique et
	                en Automatique, Jade Alglave, and Luc Maranget)

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

This file derives from the Herd7 project
(https://github.com/herd/herdtools7); its original attribution and
copyright notice follow. *)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Core
open X86Ast

let parse_reg (s : string) : reg option =
  RegTable.of_string s

type parse_error =
  | Statement
  | Instruction
  | Operand

let print_parse_error =
  function
  | Statement -> "statement"
  | Instruction -> "instruction"
  | Operand -> "operand"

let pp_error f err = Format.pp_print_string f (print_parse_error err)

exception ParseError of ((Lexing.position * Lexing.position) * parse_error)

let maybe_int_of_string s =
  try
    Some (Int.of_string s)
  with _ -> None

let is_program_label l =
  (* TODO(@MattWindsor91): this is probably GCC-specific. *)
  String.chop_prefix ~prefix:"_P" l
  |> Option.exists ~f:(fun ls -> match maybe_int_of_string ls with
                                 | Some x when 0 <= x -> true
                                 | _ -> false)

let%expect_test "is_program_label: positive example" =
  printf "%b" (is_program_label "_P0");
  [%expect {| true |}]

let%expect_test "is_program_label: wrong suffix" =
  printf "%b" (is_program_label "_P0P");
  [%expect {| false |}]

let%expect_test "is_program_label: negative" =
  printf "%b" (is_program_label "_P-1");
  [%expect {| false |}]
