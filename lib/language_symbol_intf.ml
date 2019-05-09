(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Language interface: Symbols

    This module contains the interface act languages must implement for
    symbol analysis, [Basic]; the expanded interface the rest of act gets,
    [S]; and a functor from one to the other, [Make]. *)

open Core_kernel
open Utils

(** [Basic] is the interface act languages must implement for symbol
    analysis. *)
module type Basic = sig
  (** [t] is the concrete type of symbols. *)
  type t [@@deriving compare, eq, sexp]

  (** Languages must supply a pretty-printer for their symbols. *)
  include Pretty_printer.S with type t := t

  (** They must allow traversal over any string parts of the symbol. *)
  module On_strings :
    Travesty.Traversable.S0 with type Elt.t = String.t and type t = t

  val abstract : t -> Abstract.Symbol.t
  (** [abstract sym] promotes [sym] to an abstract symbol without any
      reverse-engineering. *)

  val abstract_demangle : t -> Abstract.Symbol.t list
  (** [abstract_demangle sym] tries to reverse any compiler mangling of
      [sym], returning a list of abstract symbols that correspond to
      possible original values for [sym].

      The list should be in decreasing order of likelihood, and other
      heuristics can depend on this order. *)

  val to_string : t -> string
  (** [to_string t] converts a [t] to a string. The string should be a valid
      assembly-level representation of [t]. *)

  val require_of_string : string -> t Or_error.t
  (** [require_of_string t] tries to interpret [s] as a symbol. This
      function can return an error if the string doesn't represent a legal
      symbol in this language. *)
end

(** [S] is an expanded interface onto an act language's symbol analysis. *)
module type S = sig
  include Basic

  (** [Set] is an extended set module for concrete symbols. *)
  module Set : sig
    include Set.S with type Elt.t = t

    include My_set.Extensions with type t := t

    val abstract : t -> Abstract.Symbol.Set.t
    (** [abstract set] applies [abstract] to every symbol in the concrete
        symbol set [set]. *)
  end

  include
    Comparable.S
    with type t := t
     and type comparator_witness = Set.Elt.comparator_witness
     and module Set := Set

  (** [R_map] is an implementation of [Redirect_map] for this symbol type. *)
  module R_map : Redirect_map.S with type sym = t and type sym_set = Set.t

  val of_string_opt : string -> t option
  (** [of_string_opt t] tries to interpret [s] as a symbol. This function
      can return [None] if the string doesn't represent a legal symbol in
      this language. *)

  val program_id_of : t -> int option
  (** [program_id_of sym] tries to interpret [sym] as a program label; if
      so, it returns [Some n] where [n] is the underlying program ID.

      Program labels, conventionally, take the form [Pn] where [n] is a
      non-negative integer. Since the compiler may have mangled the label,
      [program_id_of] relies on [abstract_demangle]. *)

  (** {3 Predicates on symbols} *)

  val is_program_label : t -> bool
  (** [is_program_label sym] determines whether concrete symbol [sym] is a
      program label; it is equivalent to
      [Option.is_some (program_name_of sym)]. *)

  val is_c_safe : t -> bool
  (** [is_c_safe sym] checks whether [sym]'s string representation is safe
      for use in C as-is. *)

  val is_herd_safe : t -> bool
  (** [is_herd_safe sym] checks whether [sym]'s string representation is
      safe for use in Herd as-is. *)
end
