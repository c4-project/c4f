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
open Utils

(** [syntax] marks an x86 AST as being either AT&T or Intel syntax. *)

type syntax =
  | SynAtt
  | SynIntel

(** [SyntaxMap] associates each syntax type with its string name. *)
module SyntaxMap : (StringTable.Intf with type t = syntax)

(** [sexp_of_syntax syn] converts [syn] into an S-expression. *)
val sexp_of_syntax : syntax -> Sexp.t
(** [syntax_of_sexp sexp] tries to interpret an S-expression [sexp] as
   a syntax name. *)
val syntax_of_sexp : Sexp.t -> syntax

(** [reg] enumerates all of the known (32-bit) x86 registers. *)
type reg =
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | AX | BX | CX | DX
  | AL | BL | CL | DL
  | AH | BH | CH | DH
  | ZF | SF | CF

(** [RegTable] associates each register with its string name. *)
module RegTable : (StringTable.Intf with type t = reg)

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

type bop =
  | BopPlus
  | BopMinus

type operand =
  | OperandIndirect of indirect
  | OperandReg of reg
  | OperandImmediate of disp
  | OperandString of string
  | OperandBop of operand * bop * operand

type prefix =
  | PreLock

type size =
  | X86SByte
  | X86SWord
  | X86SLong

(** [inv_condition] enumerates all of the x86 conditions that can be
   inverted with a 'not' prefix (for example, 'above' (A) becomes 'not
   above' (NA). *)
type inv_condition =
  [ `Above
  | `AboveEqual
  | `Below
  | `BelowEqual
  | `Carry
  | `Equal
  | `Greater
  | `GreaterEqual
  | `Less
  | `LessEqual
  | `Overflow
  | `Parity
  | `Sign
  | `Zero
  ]

(** [condition] enumerates all x86 conditions, including both forms of
    invertible conditions. *)
type condition =
  [ inv_condition
  | `Not of inv_condition
  | `CXZero
  | `ECXZero
  | `ParityEven
  | `ParityOdd
  ]

(** [opcode] enumerates X86 opcodes.

Some opcodes contain optional [size] parameters.  These collect any
   AT&T-style size suffixes that were found on the opcode during
   parsing.  The parser won't raise an error if it sees an AT&T
   suffixes when parsing Intel, or no suffix when parsing AT&T.

The parser is lax when it comes to opcodes it doesn't understand: it
   emits them as [X86OpUnknown]. *)
type opcode =
  | X86OpJump of condition option
  | X86OpMov of size option
  | X86OpNop
  | X86OpDirective of string (* Assembler directive *)
  | X86OpUnknown of string (* An opcode we don't (yet?) understand. *)

(** [OpcodeTable] associates each opcode with its string name. *)
module OpcodeTable : (StringTable.Intf with type t = opcode)

type instruction =
  { prefix   : prefix option
  ; opcode   : opcode
  ; operands : operand list
  }

type statement =
  | StmInstruction of instruction
  | StmLabel of string
  | StmNop

(** [fold_map_statement_symbols ~init ~f s] maps [f] across all
   identifier symbols in [s] (labels, memory locations, etc.),
   threading through an accumulator with initial value [~init].

It does *not* map [f] over string literals, opcodes, or directive
   names. *)
val fold_map_statement_symbols : f:('a -> string -> ('a * string)) ->
                                 init:'a ->
                                 statement ->
                                 ('a * statement)
