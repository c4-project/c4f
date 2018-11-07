(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique
   et en Automatique, Jade Alglave, and Luc Maranget)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

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

(** [Opcode] contains the x86 opcode tables, and operations on them. *)

open Lib
open Utils

(** [Sizable] enumerates X86 opcodes that may have an associated size
    directive (Intel) or suffix (AT&T). *)
module Sizable : sig
  type t =
    [ `Add
    | `Call
    | `Cmp
    | `Mov
    | `Pop
    | `Push
    | `Ret
    | `Sub
    | `Xchg
    | `Xor
    ]
  [@@deriving sexp, eq]
  ;;

  (** [Sizable] contains a string table for sizable opcodes.
      These strings _don't_ have a size suffix attached; to parse
      or emit an AT&T-style opcode with size suffix, use
      [Sized]. *)
  include StringTable.Intf with type t := t
  (** We can convert sizable opcodes to the act abstract form. *)
  include Abstractable.S with type t := t
                          and module Abs := Abstract.Instruction
end

(** [Size] contains an enumeration of operand sizes. *)
module Size : sig
  type t =
    | Byte
    | Word
    | Long
  [@@deriving sexp, eq]
  ;;

  (** [Size] contains a string table for AT&T-style size suffixes. *)
  module Suffix_table : StringTable.Intf with type t := t
end

module Sized : sig
  (** A [Sized.t] is a pair of sizable instruction and
      actual size. *)
  type t = (Sizable.t * Size.t)
  [@@deriving sexp, eq]
  ;;

  (** [Sized] contains a string table for AT&T-style size-suffixed
      opcodes. *)
  include StringTable.Intf with type t := t
  (** We can convert sized opcodes to the act abstract form. *)
  include Abstractable.S with type t := t
                          and module Abs := Abstract.Instruction
end

(** [Basic] enumerates 'regular' known opcodes that are neither jumps
    nor sizable. *)
module Basic : sig
  (** A [Basic.t] is either a [Sizable.t] or one of several other
      known opcodes. *)
  type t =
    [ Sizable.t
    | `Leave
    | `Mfence
    | `Nop
    ]
  [@@deriving sexp, eq]
  ;;

  (** [Basic] contains a string table for basic opcodes.
      This is a superset of [Sizable]'s string table. *)
  include StringTable.Intf with type t := t
  (** We can convert basic opcodes to the act abstract form. *)
  include Abstractable.S with type t := t
                          and module Abs := Abstract.Instruction
end

(** [Condition] describes jump conditions. *)
module Condition : sig
  (** [invertible] enumerates all of the x86 conditions that can be
      inverted with a 'not' prefix (for example, 'above' (A) becomes
      'not above' (NA). *)
  type invertible =
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
  [@@deriving sexp, eq]
  ;;

  (** [t] enumerates all x86 conditions, including both forms of
      invertible conditions. *)
  type t =
    [ invertible
    | `Not of invertible
    | `CXZero
    | `ECXZero
    | `ParityEven
    | `ParityOdd
    ]
  [@@deriving sexp, eq]
  ;;
end

(** [Jump] describes jump instructions. *)
module Jump : sig
  (** [t] is a description of a jump. *)
  type t =
    [ `Unconditional
    | `Conditional of Condition.t
    ]
  [@@deriving sexp, eq]
  ;;

  (** [Jump] contains a string table for jump instructions. *)
  include StringTable.Intf with type t := t
  (** We can convert jumps to the act abstract form. *)
  include Abstractable.S with type t := t
                          and module Abs := Abstract.Instruction
end

(** [t] enumerates all possible types of opcode. *)
type t =
  | Basic     of Basic.t  (** Instruction without AT&T size suffix *)
  | Sized     of Sized.t  (** Instruction with AT&T size suffix *)
  | Jump      of Jump.t   (** Jump instruction *)
  | Directive of string   (** Assembler directive *)
  | Unknown   of string   (** An opcode we don't (yet) understand. *)
[@@deriving sexp, eq]
;;

(** [basic b] builds a [t] from a basic opcode [b]. *)
val basic : Basic.t -> t
(** [sized s] builds a [t] from a sized opcode [s]. *)
val sized : Sized.t -> t
(** [jump j] builds a [t] from a jump opcode [j]. *)
val jump : Jump.t -> t
(** [directive name] builds a [t] from a directive with name
   [name]. *)
val directive : string -> t
(** [unknown mnemonic] builds a [t] from an unknown mnemonic
   [mnemonic]. *)
val unknown : string -> t

(** We can convert elements of [t] to the act abstract form. *)
include Abstractable.S with type t := t
                        and module Abs := Abstract.Instruction
;;
