(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
   SOFTWARE. *)

(** Language abstraction layer: statement analysis

    This module contains the interface act languages must implement
   for statement analysis, [Basic] and [Basic_with_modules]; the
   expanded interface the rest of act gets, [S]; and a functor from
   one to the other, [Make]. *)

open Core_kernel
open Utils

(** [Basic] is the interface act languages must implement for
    statement analysis. *)
module type Basic = sig
  (** [t] is the type of statements. *)
  type t [@@deriving sexp]

  (** [ins] is the type of instructions inside statements. *)
  type ins

  (** [sym] is the type of concrete symbols. *)
  type sym

  (** Languages must supply a pretty-printer for their statements. *)
  include Pretty_printer.S with type t := t

  (** They must allow traversal over symbols... *)
  module On_symbols
    : Traversable.Container0 with type elt := sym and type t := t
  ;;

  (** ...and over instructions. *)
  module On_instructions
    : Traversable.Container0 with type elt := ins and type t := t
  ;;

  include Abstract.Abstractable.S
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


(** [Basic_with_modules] extends [Basic] with the fully expanded
    language abstraction layer modules on which [Make] depends. *)
module type Basic_with_modules = sig
  module Symbol : Language_symbol.S
  module Instruction : Language_instruction.S

  include Basic with type sym := Symbol.t
                 and type ins := Instruction.t
end

(** [S] is an expanded interface onto an act language's statement
    analysis. *)
module type S = sig
  include Basic_with_modules

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

  (** We can query abstract properties
      (and, transitively, abstract instruction properties)
      directly on the concrete statement type. *)
  include Abstract.Statement.S_properties with type t := t

  (** [Extended_flag] expands [Abstract.Statement.Flag] with an
      extra flag, representing program boundaries. *)
  module Extended_flag : Abstract.Flag_enum.S
    with type t =
           [ Abstract.Statement.Flag.t | `Program_boundary ]
  ;;

  (** [extended_flags stm symbol_table] behaves like [flags], but can
     also return the new flags in [Extended_flag].  *)
  val extended_flags
    :  t
    -> Abstract.Symbol.Table.t
    -> Extended_flag.Set.t
  ;;
end

(** [Language_statement] is the interface exposed in the main mli
   file. *)
module type Language_statement = sig
  module type Basic = Basic
  module type Basic_with_modules = Basic_with_modules
  module type S = S

  (** [Make] produces an instance of [S] from an instance of
     [Basic_with_modules]. *)
  module Make :
    functor (B : Basic_with_modules)
      -> S with type t = B.t
            and module Symbol = B.Symbol
            and module Instruction = B.Instruction
end
