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

(** [Basic_statement] is the signature that must be implemented by act
   languages in regards to their statement types. *)
module type Basic_statement = sig
  (** [t] is the type of statements. *)
  type t [@@deriving sexp]

  (** [ins] is the type of instructions inside statements. *)
  type ins

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their statements. *)
  include Pretty_printer.S with type t := t

  (** They must allow fold-mapping over symbols... *)
  module On_symbols
    : Fold_map.Container0 with type elt := sym and type t := t
  ;;

  (** ...and over instructions. *)
  module On_instructions
    : Fold_map.Container0 with type elt := ins and type t := t
  ;;

  include Abstractable.S
    with type t := t
     and module Abs := Abstract.Statement
  ;;

  (** [empty] builds an empty statement. *)
  val empty : unit -> t

  (** [label] builds a label with the given symbol. *)
  val label : string -> t

  (** [instruction] builds an instruction statement. *)
  val instruction : ins -> t
end

(** [Basic_instruction] is the signature that must be implemented by act
   languages in regards to their instruction types. *)
module type Basic_instruction = sig
  (** [t] is the type of instructions. *)
  type t [@@deriving sexp]

  (** [loc] is the type of locations inside instructions. *)
  type loc

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their instructions. *)
  include Pretty_printer.S with type t := t

  (** [pp_operands f ins] pretty-prints only the instruction operands. *)
  val pp_operands : Format.formatter -> t -> unit

  include Abstractable.S
    with type t := t
     and module Abs := Abstract.Instruction
  ;;

  (** They must allow fold-mapping over symbols... *)
  module On_symbols :
    Fold_map.Container0 with type elt := sym and type t := t
  ;;

  (** ...and over locations. *)
  module On_locations :
    Fold_map.Container0 with type elt := loc and type t := t
  ;;

  (** [jump sym] builds an unconditional jump to symbol [sym]. *)
  val jump : string -> t

  (** [abs_operands ins] gets the abstracted operands of instruction
     [ins]. *)
  val abs_operands : t -> Abstract.Operand.Bundle.t
end

(** [Basic_location] is the signature that must be implemented by act
   languages in regards to their location types. *)
module type Basic_location = sig
  type t [@@deriving sexp]

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their locations. *)
  include Pretty_printer.S with type t := t

  (** They must allow fold-mapping over symbols... *)
  module On_symbols :
    Fold_map.Container0 with type elt := sym and type t := t
  ;;

  include Abstractable.S
    with type t := t
     and module Abs := Abstract.Location
  ;;

  (** [make_heap_loc sym] creates a location referencing a symbolic heap
      location named [sym]. *)
  val make_heap_loc : string -> t
end

(** [Basic_constant] is the signature that must be implemented by act
   languages in regards to their constant types. *)
module type Basic_constant = sig
  type t [@@deriving sexp]

  (** Languages must supply a pretty-printer for their constants. *)
  include Pretty_printer.S with type t := t

  (** [zero] is a constant representing numeric zero. *)
  val zero : t
end

(** [Basic_core] is the signature that act languages must implement
    in regards to miscellaneous facts about the language. *)
module type Basic_core = sig
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

(** [Basic] is the signature that act languages must implement. *)
module type Basic = sig
  include Basic_core

  module Constant    : Basic_constant
  module Symbol      : Language_symbol.Basic
  module Location    : Basic_location with type sym := Symbol.t
  module Instruction : Basic_instruction with type loc := Location.t
                                          and type sym := Symbol.t
  module Statement   : Basic_statement with type ins := Instruction.t
                                        and type sym := Symbol.t
end

(** [S] is the user-facing interface module for act languages.
    Usually, you can get an [S] by applying the functor [Make]
    onto a bare-bones language [Basic]. *)
module type S = sig
  include Basic_core

  module Constant : sig
    include Basic_constant
  end

  module Symbol : Language_symbol.S

  module Location : sig
    include Basic_location with type sym := Symbol.t

    (** [to_heap_symbol l] returns [l]'s underlying abstract heap
       symbol if it is a symbolic heap reference. *)
    val to_heap_symbol : t -> Abstract.Symbol.t option
  end

  module Instruction : sig
    include Basic_instruction with type loc := Location.t
                               and type sym := Symbol.t
    (** As a convenience, we can query abstract instruction properties
        directly on the concrete instruction type.  All of these
        operations implicitly route through [abs_type] and
        [abs_operands]. *)
    include Abstract.Instruction.S_properties with type t := t

    (** We can query abstract operand bundle properties on a concrete
       instruction type, routing through [abs_operands]. *)
    module On_operands :
      Abstract_operand.Bundle.S_properties
        with type t := t
    ;;
  end

  module Statement : sig
    include Basic_statement with type ins := Instruction.t
                             and type sym := Symbol.t
    (** [is_unused_ordinary_label stm ~symbol_table] tests whether
       [stm] is an unused (not-jumped-to) label that doesn't have
       special meaning to act.  It uses [~symbol_table] in the same
        way as [is_unused_label]. *)
    val is_unused_ordinary_label
      :  t
      -> symbol_table:Abstract.Symbol.Table.t
      -> bool
    ;;

    (** [is_program_boundary stm] tests whether [stm] is a program
        boundary per act's conventions. *)
    val is_program_boundary : t -> bool

    (** As a convenience, we can query abstract statement properties
        (and, transitively, abstract instruction properties)
        directly on the concrete statement type.  All of these
        operations implicitly route through [abs_type]. *)
    include Abstract.Statement.S_properties with type t := t

    (** [Extended_flag] expands [Abstract.Statement.Flag] with an
       extra flag, representing program boundaries. *)
    module Extended_flag : Abstract_flag.S with type t =
      [ Abstract.Statement.Flag.t | `Program_boundary ]
    ;;

    (** [extended_flags stm symbol_table] behaves like
        [flags], but can also return the new flags in [Extended_flag].
    *)
    val extended_flags
      :  t
      -> Abstract.Symbol.Table.t
      -> Extended_flag.Set.t
    ;;
  end

  (** [symbols ~known_heap_symbols prog] calculates the symbol table
     for [prog].  It uses [known_heap_symbols] to help work out corner
     cases in heap symbol detection (for example, when heap references
     are being used as immediate values). *)
  val symbols
    :  Statement.t list
    -> known_heap_symbols:Abstract.Symbol.Set.t
    -> Abstract.Symbol.Table.t
  ;;
end

module type Language = sig
  module type Basic = Basic
  module type Basic_constant = Basic_constant
  module type Basic_location = Basic_location
  module type Basic_instruction = Basic_instruction
  module type Basic_statement = Basic_statement

  module type S = S

  (** [Make] builds a module satisfying [S] from one satisfying [Basic]. *)
  module Make : functor (B : Basic) ->
    S with type Constant.t    = B.Constant.t
       and type Location.t    = B.Location.t
       and type Instruction.t = B.Instruction.t
       and type Statement.t   = B.Statement.t
       and type Symbol.t      = B.Symbol.t
end
