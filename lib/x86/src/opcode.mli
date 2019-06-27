(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.

   This file derives in part from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(* the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

(** [Opcode] contains the x86 opcode tables, and operations on them. *)

open Base
open Act_common
open Act_utils

(** [Operand_spec] describes the various types of operand that x86
    instructions can have.

    These specs, and their corresponding tables in the opcode modules, tell
    [Language_instruction] which opcode types are allowed in which
    instructions.

    Jumps are a special case, and don't have operand specs; they are always
    interpreted as taking a single operand corresponding to the jump target. *)
module Operand_spec : sig
  (** [single] enumerates the possible types of operand.

      At present, we don't model operand size restrictions. *)
  type single =
    | Immediate  (** This operand can be an immediate value (imm) *)
    | Memory  (** This operand can be a memory reference (m) *)
    | Register  (** This operand can be a register (r) *)

  (** [t] describes the operands of an instruction. *)
  type t =
    | Zero  (** This instruction takes no operands. *)
    | One of single list
        (** This instruction takes one operand, which may satisfy any of the
            following specifications. *)
    | Symmetric of (single * single) list
        (** This instruction takes two operands, which may satisfy any of
            the following pairwise specifications in either order. *)
    | Src_dst of (single, single) Src_dst.t list
        (** This instruction takes two operands, which may satisfy any of
            the following pairwise specifications so long as the source
            matches the [src] side and the destination matches the [dst]
            side. *)
    | Or of t * t
        (** This instruction can match either of these descriptions. *)
end

(** [Sizable] enumerates x86 opcodes that may have an associated size
    directive (Intel) or suffix (AT&T). *)
module Sizable : sig
  type t =
    [ `Add
    | `Call
    | `Cmp
    | `Cmpxchg
    | `Mov
    | `Pop
    | `Push
    | `Ret
    | `Sub
    | `Xchg
    | `Xor ]
  [@@deriving sexp, equal]

  include
    String_table.S with type t := t
  (** [Sizable] contains a string table for sizable opcodes. These strings
      _don't_ have a size suffix attached; to parse or emit an AT&T-style
      opcode with size suffix, use [Sized]. *)

  (** We can convert sizable opcodes to the act abstract form. *)
  include
    Act_abstract.Abstractable.S
      with type t := t
       and module Abs := Act_abstract.Instruction.Opcode

  val get_operand_spec : t -> Operand_spec.t option
  (** [get_operand_spec opcode] tries to get an operand spec for [opcode].
      It returns [None] if the opcode doesn't yet have operand analysis. *)
end

(** [Size] contains an enumeration of operand sizes. *)
module Size : sig
  type t = Byte | Word | Long [@@deriving sexp, equal]

  module Suffix_table : String_table.S with type t := t
  (** [Size] contains a string table for AT&T-style size suffixes. *)
end

module Sized : sig
  type t = Sizable.t * Size.t [@@deriving sexp, equal]
  (** A [Sized.t] is a pair of sizable instruction and actual size. *)

  include
    String_table.S with type t := t
  (** [Sized] contains a string table for AT&T-style size-suffixed opcodes. *)

  (** We can convert sized opcodes to the act abstract form. *)
  include
    Act_abstract.Abstractable.S
      with type t := t
       and module Abs := Act_abstract.Instruction.Opcode
end

(** [Basic] enumerates 'regular' known opcodes that are neither jumps nor
    sizable. *)
module Basic : sig
  type t = [Sizable.t | `Leave | `Mfence | `Nop]
  [@@deriving sexp, equal, enumerate]
  (** A [Basic.t] is either a [Sizable.t] or one of several other known
      opcodes. *)

  include
    String_table.S with type t := t
  (** [Basic] contains a string table for basic opcodes. This is a superset
      of [Sizable]'s string table. *)

  (** We can convert basic opcodes to the act abstract form. *)
  include
    Act_abstract.Abstractable.S
      with type t := t
       and module Abs := Act_abstract.Instruction.Opcode

  val get_operand_spec : t -> Operand_spec.t option
  (** [get_operand_spec opcode] tries to get an operand spec for [opcode].
      It returns [None] if the opcode doesn't yet have operand analysis. *)
end

(** [Condition] describes jump conditions. *)
module Condition : sig
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
    | `Zero ]
  [@@deriving sexp, equal]
  (** [invertible] enumerates all of the x86 conditions that can be inverted
      with a 'not' prefix (for example, 'above' (A) becomes 'not above'
      (NA). *)

  type t =
    [ invertible
    | `Not of invertible
    | `CXZero
    | `ECXZero
    | `ParityEven
    | `ParityOdd ]
  [@@deriving sexp, equal]
  (** [t] enumerates all x86 conditions, including both forms of invertible
      conditions. *)
end

(** [Jump] describes jump instructions. *)
module Jump : sig
  type t = [`Unconditional | `Conditional of Condition.t]
  [@@deriving sexp, equal, enumerate]
  (** [t] is a description of a jump. *)

  include
    String_table.S with type t := t
  (** [Jump] contains a string table for jump instructions. *)

  (** We can convert jumps to the act abstract form. *)
  include
    Act_abstract.Abstractable.S
      with type t := t
       and module Abs := Act_abstract.Instruction.Opcode
end

(** [t] enumerates all possible types of opcode. *)
type t =
  | Basic of Basic.t  (** Instruction without AT&T size suffix *)
  | Sized of Sized.t  (** Instruction with AT&T size suffix *)
  | Jump of Jump.t  (** Jump instruction *)
  | Directive of string  (** Assembler directive *)
  | Unknown of string  (** An opcode we don't (yet) understand. *)
[@@deriving sexp, equal]

val basic : Basic.t -> t
(** [basic b] builds a [t] from a basic opcode [b]. *)

val sized : Sized.t -> t
(** [sized s] builds a [t] from a sized opcode [s]. *)

val jump : Jump.t -> t
(** [jump j] builds a [t] from a jump opcode [j]; not to be confused with
    {{!jmp} jmp (with no U)}, which specifically builds an {i unconditional}
    jump. *)

val directive : string -> t
(** [directive name] builds a [t] from a directive with name [name]. *)

val unknown : string -> t
(** [unknown mnemonic] builds a [t] from an unknown mnemonic [mnemonic]. *)

(** We can convert elements of [t] to the act abstract form. *)
include
  Act_abstract.Abstractable.S
    with type t := t
     and module Abs := Act_abstract.Instruction.Opcode

val of_string : string -> t
(** [of_string string] parses [string] as an opcode (or opcode-like entity). *)

(** {2 Common opcodes} *)

val call : t
(** [cal] is CALL. *)

val jmp : t
(** [jmp] is JMP (unconditional jump); not to be confused with
    {{!jump} jump (with a U)}, which builds jump instructions given an
    explicit jump style. *)
