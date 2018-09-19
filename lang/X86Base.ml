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

(** Define registers, barriers, and instructions for X86 *) 

open Core


(*************)
(* Registers *)
(*************)

type reg =
  (* 32-bit *)
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | AX | BX | CX | DX
  | AL | BL | CL | DL
  | AH | BH | CH | DH
  | ZF | SF | CF
(* Wastefully putting 31 bits instead of 1 bit *)
  | Symbolic_reg of string
  | Internal of int

let loop_idx = Internal 0
let sig_cell = "sig_cell"

let pc = EIP

let regs =
  [
   EAX, "EAX" ;
   EBX, "EBX" ;
   ECX, "ECX" ;
   EDX, "EDX" ;
   ESI, "ESI" ;
   EDI, "EDI" ;
   EBP, "EBP" ;
   ESP, "ESP" ;
   EIP, "EIP" ;
   ZF, "ZF" ;
   SF, "SF" ;
   CF, "CF" ;
   (* 16-bit registers *)
   AX, "AX";
   BX, "BX";
   CX, "CX";
   DX, "DX";
   (* 8-bit low registers *)
   AL, "AL";
   BL, "BL";
   CL, "CL";
   DL, "DL";
   (* 8-bit high registers *)
   AH, "AH";
   BH, "BH";
   CH, "CH";
   DH, "DH";
 ]

let parse_list = Map.of_alist_exn (module String.Caseless) (List.Assoc.inverse regs)

let parse_reg (s : string) : reg option =
  Map.find parse_list s

type disp =
  | DispSymbolic of string
  | DispNumeric of int

type indirect =
  { in_seg    : reg option
  ; in_disp   : disp option
  ; in_base   : reg option
  ; in_index  : reg option
  ; in_scale  : int option
  }

let in_zero () =
  { in_seg    = None
  ; in_disp   = None
  ; in_base   = None
  ; in_index  = None
  ; in_scale  = None
  }

type bop =
  | Bop_plus
  | Bop_minus

type operand =
  | Operand_indirect of indirect
  | Operand_reg of reg
  | Operand_immediate of disp
  | Operand_string of string
  | Operand_bop of operand * bop * operand

type directive =
  { dir_name : string
  ; dir_ops  : operand list
  }

type prefix =
  | Pre_Lock

type instruction =
  { prefix   : prefix option
  ; opcode   : string
  ; operands : operand list
  }

type statement =
  | Stm_label of string
  | Stm_directive of directive
  | Stm_instruction of instruction
  | Stm_nop

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

