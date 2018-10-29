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
 * Interface for language implementation
 *)

(** [BaseS] is the top-level signature that must be implemented by act
    languages (as part of [S]). *)
module type BaseS = sig
  (** [name] is the Herd7 name of the language. *)
  val name : string

  (** [pp_comment ~pp f k] prints a line comment whose body is
      given by invoking [pp] on [k]. *)
  val pp_comment
    :  pp:(Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a
    -> unit
end

(** [SymbolS], part of the interface an act target language must
    implement, concerns symbols. *)
module type SymbolS = sig
  (** [t] is the concrete type of symbols. *)
  type t [@@deriving compare, sexp]

  (** Languages must supply a pretty-printer for their symbols. *)
  include Core.Pretty_printer.S with type t := t

  (** They must allow fold-mapping over any string parts of the
      symbol. *)
  module OnStringsS
    : FoldMap.S with type t = string
                 and type cont = t
  ;;

  (** [abstract sym] promotes [sym] to an abstract symbol without any
      reverse-engineering. *)
  val abstract : t -> Abstract.Symbol.t

  (** [abstract_demangle sym] tries to reverse any compiler mangling of
      [sym], returning a list of abstract symbols that correspond to
      possible original values for [sym].

      The list should be in decreasing order of likelihood, and
      other heuristics can depend on this order. *)
  val abstract_demangle : t -> Abstract.Symbol.t list
end

(** [StatementS] is the signature that must be implemented by
    act languages in regards to their statement types. *)
module type StatementS = sig
  (** [t] is the type of statements. *)
  type t

  (** [ins] is the type of instructions inside statements. *)
  type ins

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their statements. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** They must allow fold-mapping over symbols... *)
  module OnSymbolsS
    : FoldMap.S with type t = sym
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
  val abs_type : t -> Abstract.Statement.t
end

module type InstructionS = sig
  (** [t] is the type of instructions. *)
  type t

  (** [loc] is the type of locations inside instructions. *)
  type loc

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their instructions. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** They must allow fold-mapping over symbols... *)
  module OnSymbolsS
    : FoldMap.S with type t = sym
                 and type cont = t

  (** ...and over locations. *)
  module OnLocationsS
    : FoldMap.S with type t = loc
                 and type cont = t

  (** [jump sym] builds an unconditional jump to symbol [sym]. *)
  val jump : string -> t

  (** [operands ins] gets the abstracted operands of instruction
      [ins]. *)
  val abs_operands : t -> Abstract.Operands.t

  (** [abs_type ins] gets the abstract type of instruction [ins]. *)
  val abs_type : t -> Abstract.Instruction.t
end

module type LocationS = sig
  type t

  (** Languages must supply a pretty-printer for their locations. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** [make_heap_loc sym] creates a location referencing a symbolic heap
      location named [sym]. *)
  val make_heap_loc : string -> t

  (** [abs_type loc] gets the abstract type of a location. *)
  val abs_type : t -> Abstract.Location.t
end

module type ConstantS = sig
  type t

  (** Languages must supply a pretty-printer for their constants. *)
  include Core.Pretty_printer.S with type t := t

  (** They must also include S-expression conversion. *)
  include Sexpable.S with type t := t

  (** [zero] is a constant representing numeric zero. *)
  val zero : t
end

(** [S] is the signature that Litmus languages must implement. *)
module type S = sig
  include BaseS
  module Constant    : ConstantS
  module Location    : LocationS
  module Symbol      : SymbolS
  module Instruction : InstructionS with type loc = Location.t
                                     and type sym = Symbol.t
  module Statement   : StatementS with type ins = Instruction.t
                                   and type sym = Symbol.t
end

(*
 * End-user interface
 *)

(** [Intf] is the user-facing interface module for act languages.
    Usually, you can get an [Intf] by applying the functor [Make]
    onto a bare-bones language [S]. *)
module type Intf = sig
  include BaseS

  module Constant : sig
    include ConstantS
  end

  module Symbol : sig
    include SymbolS

    module Set : sig
      include Set.S with type Elt.t = t
      include MyContainers.SetExtensions with type t := t

      (** [abstract set] applies [abstract] to every symbol in
          the concrete symbol set [set]. *)
      val abstract : t -> Abstract.Symbol.Set.t
    end

    (** [OnStrings] is an extension of the incoming
        [SymbolS.OnStringsS]. *)
    module OnStrings
      : FoldMap.SetIntf with type t = string
                         and type cont = t
    ;;

    (** [program_id_of sym] tries to interpret [sym] as a
        program label; if so, it returns [Some n] where [n]
        is the underlying program ID.

        Program labels, conventionally, take the form
        [Pn] where [n] is a non-negative integer.  Since the
        compiler may have mangled the label, [program_id_of]
        relies on [abstract_demangle]. *)
    val program_id_of : t -> int option

    (** [is_program_label sym] determines whether concrete symbol
        [sym] is a program label; it is equivalent to
        [Option.is_some (program_name_of sym)]. *)
    val is_program_label : t -> bool
  end

  module Location : sig
    include LocationS

    (** [to_heap_symbol l] returns [l]'s underlying abstract heap
       symbol if it is a symbolic heap reference. *)
    val to_heap_symbol : t -> Abstract.Symbol.t option
  end

  module Instruction : sig
    include InstructionS with type loc = Location.t
                          and type sym = Symbol.t

    (** [OnSymbols] is an extension of the incoming
        [InstructionS.OnSymbolsS], including symbol map operations. *)
    module OnSymbols
      : FoldMap.SetIntf with type t = Symbol.t
                         and type cont = t

    (** [OnLocations] is an extension of the incoming
        [StatementS.OnLocationsS]. *)
    module OnLocations
      : FoldMap.Intf with type t = Location.t
                      and type cont = t

    (** [mem set ins] checks whether [ins]'s abstract type is in the
        set [set]. *)
    val mem : Abstract.Instruction.Set.t -> t -> bool

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
                        and type sym = Symbol.t

    (** [OnSymbols] is an extension of the incoming
        [StatementS.OnSymbolsS], including symbol map operations. *)
    module OnSymbols
      : FoldMap.SetIntf with type t = Symbol.t
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
    val instruction_mem : Abstract.Instruction.Set.t -> t -> bool

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

    (** [is_unused_label ignore_boundaries ~syms stm] decides whether
        [stm] is a label whose symbol isn't registered as a jump
        destination in [syms].

        If [ignore_boundaries] is present and true, [is_unused_label]
        will report program boundaries as in-use, even if they aren't
        jumped to from [jsyms]. *)
    val is_unused_label
      :  ?ignore_boundaries:bool
      -> syms:Abstract.Symbol.Table.t
      -> t
      -> bool

    (** [is_jump_pair x y] returns true if [x] is a jump instruction,
        [y] is a label, and [x] is jumping to [y]. *)
    val is_jump_pair : t -> t -> bool

    (** [is_nop stm] decides whether [stm] appears to be a NOP. *)
    val is_nop : t -> bool

    (** [is_program_boundary stm] decides whether [stm] appears to mark
        a boundary between two Litmus programs. *)
    val is_program_boundary : t -> bool

    (** [flags ~syms stm] summarises the above boolean functions as
        a set of [stm_flag]s.  It uses [syms] to calculate whether
        the statement is an unused label. *)
    val flags
      :  syms:Abstract.Symbol.Table.t
      -> t
      -> Abstract.Statement.Flag.Set.t
  end

  (** [symbols] retrieves the symbol table for a given program. *)
  val symbols : Statement.t list -> Abstract.Symbol.Table.t
end

module type Language = sig
  module type S = S
  module type Intf = Intf

  (** [Make] builds a module satisfying [Intf] from a module satisfying [S]. *)
  module Make : functor (M : S) ->
    Intf with type Constant.t    = M.Constant.t
          and type Location.t    = M.Location.t
          and type Instruction.t = M.Instruction.t
          and type Statement.t   = M.Statement.t
          and type Symbol.t      = M.Symbol.t
end
