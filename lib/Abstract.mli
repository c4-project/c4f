(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

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
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Types for abstract language observations *)

open Core
open Utils

(** [S] is the baseline signature for all abstract observation
   types. *)
module type S = sig
  type t [@@deriving sexp]
  include Pretty_printer.S with type t := t
end

(** [S_enum] is an extended signature for abstract observation types
    that are also enumerations. *)
module type S_enum = sig
  include S
  include Enum.ExtensionTable with type t := t
end

(** [Instruction] contains types and utilities for abstracted
   instructions. *)
module Instruction : sig
  (** [Instruction.t] is an abstracted instruction. *)
  type t =
    | Arith   (** arithmetic *)
    | Call    (** calling-convention related instructions *)
    | Compare (** comparison *)
    | Fence   (** memory fence *)
    | Jump    (** conditional or unconditional jump *)
    | Logical (** logical operation *)
    | Move    (** move *)
    | Nop     (** no operation *)
    | Return  (** jump to caller *)
    | Rmw     (** read-modify-write *)
    | Stack   (** stack resizing and pointer manipulation *)
    | Other   (** known, but doesn't fit in these categories *)
    | Unknown (** unclassified instruction *)

  (* Why do we have a separate [Return] type, instead of classing it
     as [Call] or [Jump]?  Two reasons:

     - It has semantics roughly in between both;

     - It makes it easier for us to translate returns to
     end-of-program jumps in sanitisation.  *)

  include S_enum with type t := t
end

(** [Location] contains types and utilities for abstracted
   locations. *)
module Location : sig
  (** [AbsLocation.t] is an abstracted location. *)
  type t =
    | StackPointer       (** Stack pointer *)
    | StackOffset of int (** Absolute offset from stack pointer *)
    | Heap of string     (** Named heap location *)
    | GeneralRegister    (** General-purpose register *)
    | Unknown            (** Not known *)

  include S with type t := t
end

(** [Statement] contains types and utilities for abstracted
   statements. *)
module Statement : sig
  (** [AbsStatement.t] is an abstracted statement. *)
  type t =
    | Directive of string
    | Instruction of Instruction.t
    | Blank
    | Label of string
    | Other

  include S with type t := t

  (** [Flag] is an enumeration of various statement observations.

      Most of these flags have corresponding Boolean accessors in
     [Language.Intf.Statement]. *)
  module Flag : sig
    type t =
      [ `UnusedLabel   (* A label that doesn't appear in any jumps *)
      | `ProgBoundary  (* A label that looks like a program boundary *)
      | `StackManip    (* A statement that only serves to manipulate
                        the call stack *)
      ]

    (** [Flag] contains various enum extensions, including a [Set]
       type. *)
    include Enum.ExtensionTable with type t := t
  end
end


(** [Operands] contains types and utilities for abstracted
   operand bundles. *)
module Operands : sig
  (** [AbsOperands.t] is an abstracted operand bundle. *)
  type t =
    (* This instruction takes no operands. *)
    | None
    (* This instruction is taking a transfer from source location to
     destination location. *)
    | LocTransfer of (Location.t, Location.t) Src_dst.t
    (* This instruction is loading an integer literal, doing something
       with it, and storing into a location. *)
    | IntImmediate of (int, Location.t) Src_dst.t
    (* This instruction is jumping to the given symbol. *)
    | SymbolicJump of string
    (* Instruction has the wrong sort of operands. *)
    | Erroneous
    (* Instruction has unclassifiable operands. *)
    | Other
    (* No analysis available---the operands may or may not be valid. *)
    | Unknown

  include S with type t := t
end

module Symbol : sig
  (** Symbols are strings. *)
  type t = string

  (** [Set] is a set module for symbols. *)
  module Set : sig
    include Set.S with type Elt.t = string
    include My_set.Extensions with type t := t
  end

  (** [Sort] is a module containing an enumeration of symbol sorts. *)
  module Sort : sig
    type t =
      | Jump
      | Heap
      | Label

    include Enum.ExtensionTable with type t := t
  end

  (** [Table] is a module concerning symbol tables: many-to-many
     mappings between symbols and sorts. *)
  module Table : sig
    type elt = t
    type t

    (** [empty] is the empty table. *)
    val empty : t;;

    (** [of_sets sets] expands a symbol-set-to-sort associative list
       into a [t]. *)
    val of_sets : (Set.t, Sort.t) List.Assoc.t -> t;;

    (** [add tbl sym sort] registers [sym] as a symbol with sort
       [sort] in [tbl], returning a new table. *)
    val add : t -> elt -> Sort.t -> t;;

    (** [remove tbl sym sort] de-registers [sym] as a symbol with sort
       [sort] in [tbl], returning a new table.

        If [sym] also maps to another sort, those mappings remain. *)
    val remove : t -> elt -> Sort.t -> t;;

    (** [set_of_sorts tbl sorts] returns all symbols in [tbl] with a
       sort in [sorts], as a symbol set. *)
    val set_of_sorts : t -> Sort.Set.t -> Set.t;;

    (** [set_of_sort tbl sort] returns all symbols in [tbl] with sort
       [sort], as a symbol set. *)
    val set_of_sort : t -> Sort.t -> Set.t;;

    (** [set tbl] returns all symbols in [tbl]
       as a symbol set. *)
    val set : t -> Set.t;;
  end

  (*
   * Symbol heuristics
   *)

  (** [program_id_of sym] tries to interpret [sym] as a
      program label; if so, it returns [Some n] where [n]
      is the underlying program ID. *)
  val program_id_of : t -> int option
end
