(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Language abstraction layer: statement analysis

    This module contains the interface act languages must implement for
    statement analysis, [Basic] and [Basic_with_modules]; the expanded
    interface the rest of act gets, [S]; and a functor from one to the
    other, [Make]. *)

open Core_kernel

(** [Basic] is the interface act languages must implement for statement
    analysis. *)
module type Basic = sig
  (** The type of statements, which must be sexpable and equatable. *)
  type t [@@deriving sexp, eq]

  (** Type of instructions inside statements. *)
  module Ins : Equal.S

  (** Type of concrete symbols. *)
  module Sym : Equal.S

  (** Languages must supply a pretty-printer for their statements. *)
  include Pretty_printer.S with type t := t

  (** They must allow traversal over symbols... *)
  module On_symbols :
    Travesty.Traversable.S0 with module Elt = Sym and type t := t

  (** ...and over instructions. *)
  module On_instructions :
    Travesty.Traversable.S0 with module Elt = Ins and type t := t

  include
    Abstract.Abstractable.S
    with type t := t
     and module Abs := Abstract.Statement

  val empty : unit -> t
  (** [empty] builds an empty statement. *)

  val label : string -> t
  (** [label] builds a label with the given symbol. *)

  val instruction : Ins.t -> t
  (** [instruction] builds an instruction statement. *)
end

(** [Basic_with_modules] extends [Basic] with the fully expanded language
    abstraction layer modules on which [Make] depends. *)
module type Basic_with_modules = sig
  module Instruction : Instruction.S

  include
    Basic
    with module Sym := Instruction.Symbol
     and module Ins := Instruction
end

(** [S] is an expanded interface onto an act language's statement analysis. *)
module type S = sig
  include Basic_with_modules

  val is_unused_ordinary_label :
    t -> symbol_table:Abstract.Symbol.Table.t -> bool
  (** [is_unused_ordinary_label stm ~symbol_table] tests whether [stm] is an
      unused (not-jumped-to) label that doesn't have special meaning to act.
      It uses [~symbol_table] in the same way as [is_unused_label]. *)

  val is_program_boundary : t -> bool
  (** [is_program_boundary stm] tests whether [stm] is a program boundary
      per act's conventions. *)

  (** We can query abstract properties (and, transitively, abstract
      instruction properties) directly on the concrete statement type. *)
  include Abstract.Statement.S_properties with type t := t

  (** [Extended_flag] expands [Abstract.Statement.Flag] with an extra flag,
      representing program boundaries. *)
  module Extended_flag :
    Abstract.Flag_enum.S
    with type t = [Abstract.Statement.Flag.t | `Program_boundary]

  val extended_flags : t -> Abstract.Symbol.Table.t -> Extended_flag.Set.t
  (** [extended_flags stm symbol_table] behaves like [flags], but can also
      return the new flags in [Extended_flag]. *)

  val make_uniform : t list list -> t list list
  (** [make_uniform listings] pads each listing in [listing] to the same
      length, using [empty ()] as the filler. *)
end
