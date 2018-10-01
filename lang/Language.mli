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

(** High-level interface to languages supported by act *)

open Core
open Utils

(** [name] enumerates all languages, and dialects thereof, supported by act. *)
type name =
  | X86 of X86Dialect.t
         [@@deriving sexp]

(** [pp_name ?show_sublang f l] pretty-prints language name [l] onto
   formatter [f].  If [show_sublang] is true (the default), we also
   print details about any sub-language (eg. AT&T/Intel syntax for
   X86). *)
val pp_name : ?show_sublang:bool -> Format.formatter -> name -> unit

(*
 * Types for abstract language observations
 *)

(** [AbsInstruction] contains types and utilities for abstracted
   instructions. *)
module AbsInstruction :
sig
  (** [AbsInstruction.t] is an abstracted instruction. *)
  type t =
    | Arith (* arithmetic *)
    | Fence (* memory fence *)
    | Jump  (* conditional or unconditional jump *)
    | Move  (* move *)
    | Nop   (* no operation *)
    | Call  (* calling-convention related instructions *)
    | Stack (* stack resizing and pointer manipulation *)
    | Other (* unclassified instruction *)
[@@deriving enum]

  (** Abstract instructions may be pretty-printed. *)
  include Pretty_printer.S with type t := t

  (** [Set] is a set module for abstract instruction types. *)
  module Set : (Set.S with type Elt.t = t)
end

(** [AbsLocation] contains types and utilities for abstracted
   locations. *)
module AbsLocation :
sig
  (** [AbsLocation.t] is an abstracted location. *)
  type t =
    | StackPointer       (* Stack pointer *)
    | StackOffset of int (* Absolute offset from stack pointer *)
    | Heap of string     (* Named heap location *)
    | GeneralRegister    (* General-purpose register *)
    | Unknown            (* Not known *)

  (** Abstract locations may be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** [AbsStatement] contains types and utilities for abstracted
   statements. *)
module AbsStatement :
sig
  (** [AbsStatement.t] is an abstracted statement. *)
  type t =
    | Directive of string
    | Instruction of AbsInstruction.t
    | Blank
    | Label of string
    | Other

  (** Abstract statements may be pretty-printed. *)
  include Pretty_printer.S with type t := t

  (** [flag] is an enumeration of various statement observations.

Most of these flags have corresponding Boolean accessors in
     [Language.Intf.Statement]. *)
  type flag =
    [ `UnusedLabel   (* A label that doesn't appear in any jumps *)
    | `ProgBoundary  (* A label that looks like a program boundary *)
    ] [@@deriving enum, sexp]

  (** [FlagTable] associates each [flag] with a string. *)
  module FlagTable : StringTable.Intf with type t = flag

  (** [FlagSet] is a set module for [flag]. *)
  module FlagSet : sig
    include Set.S with type Elt.t := flag
    include Pretty_printer.S with type t := t
  end
end


(** [AbsOperands] contains types and utilities for abstracted
   operand bundles. *)
module AbsOperands :
sig
  (** [AbsOperands.t] is an abstracted operand bundle. *)
  type t =
    (* This instruction takes no operands. *)
    | None
    (* This instruction is taking a transfer from source location to
     destination location. *)
    | LocTransfer of (AbsLocation.t, AbsLocation.t) SrcDst.t
    (* Instruction has the wrong sort of operands. *)
    | Erroneous
    (* No analysis available---the operands may or may not be valid. *)
    | Other

  (** Abstract operand bundles may be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** [SymSet] is a set module for symbols. *)
module SymSet : (Set.S with type Elt.t = string)

(*
 * Interface for language implementation
 *)

(** [BaseS] is the top-level signature that must be implemented by act
   languages (as part of [S]). *)
module type BaseS = sig
  (** [name] is the name of the language. *)
  val name : name

  (** [is_program_label l] decides whether symbol [l] is a label
     marking the start of a program. *)
  val is_program_label : string -> bool
end

(** [StatementS] is the signature that must be implemented by
    act languages in regards to their statement types. *)
module type StatementS = sig
  type t

  (** Languages must supply a pretty-printer for their statements. *)
  include Core.Pretty_printer.S with type t := t

  (** [fold_map_symbols ~init ~f stm] maps [f] over every symbol in
     [stm], threading through an accumulator with initial value
     [init].

     There are no guarantees on order. *)
  val fold_map_symbols : f:('a -> string -> 'a * string) ->
                         init:'a ->
                         t ->
                         ('a * t)


  (** [instruction_operands stm] tries to get abstract information
       about the operands of an instruction.  If [stm] isn't an
       instruction, it returns [None].  *)
  val instruction_operands : t -> AbsOperands.t option

  (** [nop] builds a no-op instruction. *)
  val nop : unit -> t

  (** [abs_type stm] gets the abstract type of a statement. *)
  val abs_type : t -> AbsStatement.t
end

module type LocationS = sig
  type t

  (** Languages must supply a pretty-printer for their locations. *)
  include Core.Pretty_printer.S with type t := t

  (** [abs_type] gets the abstract type of a location. *)
  val abs_type : t -> AbsLocation.t
end

module type ConstantS = sig
  type t
  include Core.Pretty_printer.S with type t := t
end

(** [S] is the signature that Litmus languages must implement. *)
module type S = sig
  include BaseS
  module Statement : StatementS
  module Location : LocationS
  module Constant : ConstantS
end

(** [Intf] is the user-facing interface module for act languages.
    Usually, you can get an [Intf] by applying the functor [Make]
    onto a bare-bones language [S]. *)
module type Intf = sig
  include BaseS

  module Statement : sig
    include StatementS

    (** [map_symbols ~f stm] maps [f] over every symbol in [stm].

     There are no guarantees on order. *)
    val map_symbols : f:(string -> string) ->
                      t ->
                      t

    (** [symbol_set] retrieves the set of all symbols found in a given
       statement. *)
    val symbol_set : t -> SymSet.t

    (*
     * Instruction analysis
     *)

    (** [instruction_type stm] gets the abstract type of an instruction.
     If [stm] isn't an instruction, it returns [None].  *)
    val instruction_type : t -> AbsInstruction.t option

    (** [instruction_mem set stm] checks whether [stm] is an
       instruction and, if so, whether its abstract type is in the set
       [set]. *)
    val instruction_mem : AbsInstruction.Set.t -> t -> bool

    (*
     * Statement analysis
     *)

    (** [is_directive stm] decides whether [stm] appears to be an
     assembler directive. *)
    val is_directive : t -> bool

    (** [is_jump stm] decides whether [stm] appears to be a
     jump instruction. *)
    val is_jump : t -> bool

    (** [is_label stm] decides whether [stm] appears to be an
     label. *)
    val is_label : t -> bool

    (** [is_unused_label ~jsyms stm] decides whether [stm] is a label
       whose symbol doesn't appear in the given set [jsyms] of jump
       destination symbols. *)
    val is_unused_label : jsyms:SymSet.t -> t -> bool

    (** [is_nop stm] decides whether [stm] appears to be a NOP. *)
    val is_nop : t -> bool

    (** [is_program_boundary stm] decides whether [stm] appears to mark
      a boundary between two Litmus programs. *)
    val is_program_boundary : t -> bool

    (** [flags ~jsyms stm] summarises the above boolean functions as
        a set of [stm_flag]s.  It uses [jsyms] to calculate whether
        the statement is an unused label. *)
    val flags : jsyms:SymSet.t -> t -> AbsStatement.FlagSet.t
  end

  module Location : sig
    include LocationS
  end

  module Constant : sig
    include ConstantS
  end

  (** [jump_symbols] retrieves the set of all symbols that appear to be
      jump targets. *)
  val jump_symbols : Statement.t list -> SymSet.t
end

(** [Make] builds a module satisfying [Intf] from a module satisfying [S]. *)
module Make : functor (M : S) ->
              (Intf with type Statement.t = M.Statement.t
                     and type Location.t  = M.Location.t
                     and type Constant.t  = M.Constant.t)
