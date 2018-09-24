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

let parse_list = Map.of_alist_exn (module String.Caseless) (List.Assoc.inverse X86Ast.regs)

let parse_reg (s : string) : reg option =
  Map.find parse_list s

let in_zero () =
  { in_seg    = None
  ; in_disp   = None
  ; in_base   = None
  ; in_index  = None
  }

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
  |> Option.exists ~f:(fun ls -> maybe_int_of_string ls <> None)

module ATT : (Language.S with type statement = X86Ast.statement) = struct
  let name = (Language.X86 (X86Ast.SynAtt))

  type statement = X86Ast.statement
  type constant = X86Ast.operand (* TODO: this is too weak *)
  type location = X86Ast.indirect (* TODO: as is this *)

  let pp_statement = X86Ast.pp_statement X86Ast.SynAtt
  let pp_constant = X86Ast.pp_operand X86Ast.SynAtt
  let pp_location = X86Ast.pp_indirect X86Ast.SynAtt

  let nop () = X86Ast.StmNop

  let is_directive =
    function
    | X86Ast.StmDirective _ -> true
    | _ -> false

  let is_program_boundary =
    function
    | X86Ast.StmLabel l -> is_program_label l
    | _ -> false
end
