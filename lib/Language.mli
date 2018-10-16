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

(*
 * Types for abstract language observations
 *)

(** [AbsInstruction] contains types and utilities for abstracted
   instructions. *)
module AbsInstruction : sig
  (** [AbsInstruction.t] is an abstracted instruction. *)
  type t =
    | Arith   (* arithmetic *)
    | Call    (* calling-convention related instructions *)
    | Compare (* comparison *)
    | Fence   (* memory fence *)
    | Jump    (* conditional or unconditional jump *)
    | Move    (* move *)
    | Nop     (* no operation *)
    | Return  (* jump to caller -- see below *)
    | Stack   (* stack resizing and pointer manipulation *)
    | Other   (* known, but doesn't fit in these categories *)
    | Unknown (* unclassified instruction *)

  (* Why do we have a separate [Return] type, instead of classing it
     as [Call] or [Jump]?  Two reasons:

     - It has semantics roughly in between both;

     - It makes it easier for us to translate returns to
     end-of-program jumps in sanitisation.  *)

  (** [AbsInstruction] contains various enum extensions, including a
      [Set] type. *)
  include Enum.ExtensionTable with type t := t
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
module AbsStatement : sig
  (** [AbsStatement.t] is an abstracted statement. *)
  type t =
    | Directive of string
    | Instruction of AbsInstruction.t
    | Blank
    | Label of string
    | Other

  (** Abstract statements may be pretty-printed. *)
  include Pretty_printer.S with type t := t

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
    (* This instruction is loading an integer literal, doing something
       with it, and storing into a location. *)
    | IntImmediate of (int, AbsLocation.t) SrcDst.t
    (* This instruction is jumping to the given symbol. *)
    | SymbolicJump of string
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
  (** [name] is the Herd7 name of the language. *)
  val name : string

  (** [is_program_label l] decides whether symbol [l] is a label
     marking the start of a program. *)
  val is_program_label : string -> bool
end

(** [StatementS] is the signature that must be implemented by
    act languages in regards to their statement types. *)
module type StatementS = sig
  (** [t] is the type of statements. *)
  type t

  (** [ins] is the type of instructions inside statements. *)
  type ins

  (** Languages must supply a pretty-printer for their statements. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** They must allow fold-mapping over symbols... *)
  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t

  (** ...and over instructions. *)
  module OnInstructionsS
    : FoldMap.S with type t = ins
                 and type cont = t

  (*
   * Constructors
   *)

  (** [empty] builds an empty statement. *)
  val empty : unit -> t

  (** [label] builds a label with the given symbol. *)
  val label : string -> t

  (** [instruction] builds an instruction statement. *)
  val instruction : ins -> t

  (*
   * Inspection
   *)

  (** [abs_type stm] gets the abstract type of a statement. *)
  val abs_type : t -> AbsStatement.t
end

module type InstructionS = sig
  (** [t] is the type of instructions. *)
  type t

  (** [loc] is the type of locations inside instructions. *)
  type loc

  (** Languages must supply a pretty-printer for their instructions. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t


  (** They must allow fold-mapping over symbols... *)
  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t

  (** ...and over locations. *)
  module OnLocationsS
    : FoldMap.S with type t = loc
                 and type cont = t

  (** [jump sym] builds an unconditional jump to symbol [sym]. *)
  val jump : string -> t

  (** [operands ins] gets the abstracted operands of instruction
     [ins]. *)
  val abs_operands : t -> AbsOperands.t

  (** [abs_type ins] gets the abstract type of instruction [ins]. *)
  val abs_type : t -> AbsInstruction.t
end

module type LocationS =
sig
  type t

  (** Languages must supply a pretty-printer for their locations. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** [make_heap_loc sym] creates a location referencing a symbolic heap
      location named [sym]. *)
  val make_heap_loc : string -> t

  (** [abs_type loc] gets the abstract type of a location. *)
  val abs_type : t -> AbsLocation.t
end

module type ConstantS =
sig
  type t

  (** Languages must supply a pretty-printer for their constants. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** [zero] is a constant representing numeric zero. *)
  val zero : t
end

(** [S] is the signature that Litmus languages must implement. *)
module type S =
sig
  include BaseS
  module Constant : ConstantS
  module Location : LocationS
  module Instruction : InstructionS with type loc = Location.t
  module Statement : StatementS with type ins = Instruction.t
end

(** [Intf] is the user-facing interface module for act languages.
    Usually, you can get an [Intf] by applying the functor [Make]
    onto a bare-bones language [S]. *)
module type Intf = sig
  include BaseS

  module Constant : sig
    include ConstantS
  end

  module Location : sig
    include LocationS

    (** [to_heap_symbol l] returns [l]'s underlying heap symbol if it
       is a symbolic heap reference. *)
    val to_heap_symbol : t -> string option
  end

  module Instruction : sig
    include InstructionS

    (** [OnSymbols] is an extension of the incoming
        [InstructionS.OnSymbolsS], including symbol map operations. *)
    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t

    (** [OnLocations] is an extension of the incoming
        [StatementS.OnLocationsS]. *)
    module OnLocations
      : FoldMap.Intf with type t = Location.t
                      and type cont = t

    (** [mem set ins] checks whether [ins]'s abstract type is in the
       set [set]. *)
    val mem : AbsInstruction.Set.t -> t -> bool

    (** [is_jump ins] decides whether [ins] appears to be a
     jump instruction. *)
    val is_jump : t -> bool

    (** [is_stack_manipulation ins] decides whether [ins] is a
        stack manipulation, and therefore can be removed in litmus
        tests. *)
    val is_stack_manipulation : t -> bool
  end

  module Statement : sig
    include StatementS with type ins = Instruction.t

    (** [OnSymbols] is an extension of the incoming
        [StatementS.OnSymbolsS], including symbol map operations. *)
    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t

    (** [OnInstructions] is an extension of the incoming
        [StatementS.OnInstructionsS]. *)
    module OnInstructions
      : FoldMap.Intf with type t = Instruction.t
                      and type cont = t

    (*
     * Shortcuts for querying instructions in a statement
     *)

    (** [instruction_mem set stm] checks whether [stm] is an
       instruction and, if so, whether its abstract type is in the set
       [set]. *)
    val instruction_mem : AbsInstruction.Set.t -> t -> bool

    (** [is_jump stm] decides whether [stm] appears to be a
     jump instruction. *)
    val is_jump : t -> bool

    (** [is_stack_manipulation stm] decides whether [stm] is a
        stack manipulation, and therefore can be removed in litmus
        tests. *)
    val is_stack_manipulation : t -> bool

    (*
     * Statement analysis
     *)

    (** [is_directive stm] decides whether [stm] appears to be an
     assembler directive. *)
    val is_directive : t -> bool

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
    val flags : jsyms:SymSet.t -> t -> AbsStatement.Flag.Set.t
  end

  (** [heap_symbols] retrieves the set of all symbols that appear to be
      standing in for heap locations. *)
  val heap_symbols : Statement.t list -> SymSet.t

  (** [jump_symbols] retrieves the set of all symbols that appear to be
      jump targets. *)
  val jump_symbols : Statement.t list -> SymSet.t
end

(** [Make] builds a module satisfying [Intf] from a module satisfying [S]. *)
module Make : functor (M : S) ->
  Intf with type Constant.t    = M.Constant.t
        and type Location.t    = M.Location.t
        and type Instruction.t = M.Instruction.t
        and type Statement.t   = M.Statement.t

