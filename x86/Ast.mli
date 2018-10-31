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

open Utils

(** [Reg] contains types and functions for dealing with x86 registers
   in the abstract syntax. *)
module Reg : sig
  (** [t] enumerates all commonly used registers available in 32-bit
     x86. *)
  type t =
    | AH | AL | AX | EAX
    | BH | BL | BX | EBX
    | CH | CL | CX | ECX
    | DH | DL | DX | EDX
    | BP | EBP
    | SI | ESI
    | DI | EDI
    | SP | ESP
    | CS | DS | SS | ES | FS | GS
    | CF | PF | AF | ZF | SF | OF
    | EIP
  ;;

  include Enum.ExtensionTable with type t := t

  (** [kind] enumerates the general kinds of register. *)
  type kind =
    | Gen8 of [`Low | `High]  (** General-purpose 8bit  (xH or xL) *)
    | Gen16                   (** General-purpose 16bit (usually xX) *)
    | Gen32                   (** General-purpose 32bit (usually ExX) *)
    | Segment                 (** Segment register *)
    | Flags                   (** Flag from the flag register *)
    | IP                      (** Instruction pointer *)
  ;;

  (** [kind_of r] gets the kind of register [r]. *)
  val kind_of : t -> kind
end

(** [Disp] concerns displacements. *)
module Disp : sig
  type t =
    | Symbolic of string
    | Numeric of int
  [@@deriving sexp, eq]
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     a displacement. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

(** [Index] concerns index-scale pairs. *)
module Index : sig
  type t =
    | Unscaled of Reg.t
    | Scaled of Reg.t * int
  [@@deriving sexp, eq]
  ;;

  (** [On_registers] permits enumerating and folding over registers
     inside a displacement. *)
  module On_registers
    : Fold_map.S0 with type t := t and type elt := Reg.t
  ;;
end

module Indirect : sig
  (** [t] is the opaque type of indirect memory accesses. *)
  type t [@@deriving eq]

  (** [make ?seg ?disp ?base ?index ()] makes an [Indirect] with
      the given fields (if present). *)
  val make
    :  ?seg   : Reg.t
    -> ?disp  : Disp.t
    -> ?base  : Reg.t
    -> ?index : Index.t
    -> unit
    -> t
  ;;

  (** [base] gets the indirect base, if any. *)
  val base : t -> Reg.t option;;

  (** [seg] gets the indirect segment, if any. *)
  val seg : t -> Reg.t option;;

  (** [disp] gets the indirect displacement, if any. *)
  val disp : t -> Disp.t option;;

  (** [index] gets the indirect index, if any. *)
  val index : t -> Index.t option;;

  (** [On_registers] permits enumerating and folding over registers
     inside a memory access. *)
  module On_registers
    : Fold_map.S0 with type t := t and type elt := Reg.t
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     a memory access. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

(** [Location] enumerates memory locations: either
    indirect seg/disp/base/index stanzas, or registers. *)
module Location : sig
  type t =
    | Indirect of Indirect.t
    | Reg of Reg.t
  [@@deriving sexp, eq]

  (** [On_registers] permits enumerating and folding over registers
     inside a location. *)
  module On_registers
    : Fold_map.S0 with type t := t and type elt := Reg.t
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     a location. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

module Operand : sig
  type bop =
    | BopPlus
    | BopMinus
  [@@deriving sexp, eq]
  ;;

  type t =
    | Location of Location.t
    | Immediate of Disp.t
    | String of string
    | Typ of string (* Type annotation *)
    | Bop of t * bop * t
  [@@deriving sexp, eq]
  ;;

  (* Constructors *)

  val location : Location.t -> t
  val immediate : Disp.t -> t
  val string : string -> t
  val typ : string -> t
  val bop : t -> bop -> t -> t

  (** [On_locations] permits enumerating and folding over locations
     inside an operand. *)
  module On_locations
    : Fold_map.S0 with type t := t and type elt := Location.t
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     an operand. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

type prefix =
  | PreLock
[@@deriving sexp]

type size =
  | SByte
  | SWord
  | SLong
[@@deriving sexp]

module ATTSizeTable : (StringTable.Intf with type t = size)

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
[@@deriving sexp]

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
[@@deriving sexp]

(*
 * Opcodes
 *)

(** [sizable_opcode] enumerates X86 opcodes that may have an associated size
    directive (Intel) or suffix (AT&T). *)
type sizable_opcode =
  [ `Add
  | `Call (* Some compilers seem to emit CALLQ? *)
  | `Cmp
  | `Mov
  | `Pop
  | `Push
  | `Ret (* Some compilers seem to emit RETL (32-bit)/RETQ (64-bit);
            it's unclear if there's any semantic difference from RET. *)
  | `Sub
  | `Xchg
  | `Xor
  ]
[@@deriving sexp]

(** [SizableOpcodeTable] is a parse table for [sizable_opcode]s
   *without* an associated size.

To parse an AT&T-style opcode-with-size-suffix, use
   [SizedOpcodeTable]. *)
module SizableOpcodeTable : (StringTable.Intf with type t = sizable_opcode)

(** [ATTSizedOpcodeTable] associates each pair of sizable opcode and
   size with a string.

This is mainly for AT&T-style dialects; Intel handles this
   differently. *)
module ATTSizedOpcodeTable : (StringTable.Intf with type t = (sizable_opcode * size))

(** [basic_opcode] enumerates all known X86 opcodes that aren't
   jumps. *)
type basic_opcode =
  [ sizable_opcode
  | `Leave
  | `Mfence
  | `Nop
  ]
[@@deriving sexp]

(** [BasicOpcodeTable] is a parse table for [basic_opcode]s without an
   associated size. *)
module BasicOpcodeTable : (StringTable.Intf with type t = basic_opcode)

(* [opcode] enumerates all x86 opcode-style items.  The parser is lax
   when it comes to opcodes it doesn't understand: it emits them as
   [OpUnknown]. *)
type opcode =
  | OpBasic of basic_opcode
  | OpSized of sizable_opcode * size
  | OpJump of condition option
  | OpDirective of string (* Assembler directive *)
  | OpUnknown of string (* An opcode we don't (yet?) understand. *)
[@@deriving sexp]

(** [JumpTable] is a parse table for jump opcodes. *)
module JumpTable : (StringTable.Intf with type t = condition option)

module Instruction : sig
  type t =
    { prefix   : prefix option
    ; opcode   : opcode
    ; operands : Operand.t list
    }
  [@@deriving sexp, eq, make]
  ;;

  (** [On_locations] permits enumerating and folding over locations
     inside an instruction. *)
  module On_locations
    : Fold_map.S0 with type t := t and type elt := Location.t
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     an instruction. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

module Statement : sig
  type t =
    | Instruction of Instruction.t
    | Label of string
    | Nop
  [@@deriving sexp, eq]
  ;;

  (** [instruction] creates an instruction statement. *)
  val instruction : Instruction.t -> t

  (** [On_instructions] permits enumerating and folding over
     instructions inside a statement. *)
  module On_instructions
    : Fold_map.S0 with type t := t and type elt := Instruction.t
  ;;

  (** [On_symbols] permits enumerating and folding over symbols inside
     an operand. *)
  module On_symbols
    : Fold_map.S0 with type t := t and type elt := string
  ;;
end

type t =
  { syntax  : Dialect.t
  ; program : Statement.t list
  }
[@@deriving sexp, eq, fields]
