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

(** Generic, low-level abstract syntax tree for AT&T and Intel x86 *)

open Core

(* [syntax] marks an x86 AST as being either AT&T or Intel syntax. *)

type syntax =
  | SynAtt
  | SynIntel

type reg =
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | AX | BX | CX | DX
  | AL | BL | CL | DL
  | AH | BH | CH | DH
  | ZF | SF | CF

(** [regs] associates each register with its string name. *)
val regs : (reg, string) List.Assoc.t

val pp_reg : syntax -> Format.formatter -> reg -> unit

type disp =
  | DispSymbolic of string
  | DispNumeric of int

type index =
  | Unscaled of reg
  | Scaled of reg * int

type indirect =
  { in_seg    : reg option
  ; in_disp   : disp option
  ; in_base   : reg option
  ; in_index  : index option
  }

val pp_indirect : syntax -> Format.formatter -> indirect -> unit

type bop =
  | BopPlus
  | BopMinus

val pp_bop : Format.formatter -> bop -> unit

type operand =
  | OperandIndirect of indirect
  | OperandReg of reg
  | OperandImmediate of disp
  | OperandString of string
  | OperandBop of operand * bop * operand

val pp_operand : syntax -> Format.formatter -> operand -> unit

type directive =
  { dir_name : string
  ; dir_ops  : operand list
  }

val pp_directive : syntax -> Format.formatter -> directive -> unit

type prefix =
  | PreLock

val pp_prefix : Format.formatter -> prefix -> unit

type instruction =
  { prefix   : prefix option
  ; opcode   : string
  ; operands : operand list
  }

val pp_instruction : syntax -> Format.formatter -> instruction -> unit

type statement =
  | StmLabel of string
  | StmDirective of directive
  | StmInstruction of instruction
  | StmNop

val pp_statement : syntax -> Format.formatter -> statement -> unit

val pp_ast : syntax -> Format.formatter -> statement list -> unit
