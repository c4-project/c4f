
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

(** Language interface: Symbols

    This module contains the interface act languages must implement
   for symbol analysis, [Basic]; the expanded interface the rest of
   act gets, [S]; and a functor from one to the other, [Make]. *)

open Core
open Utils

(** [Basic] is the interface act languages must implement for symbol
   analysis. *)
module type Basic = sig
  (** [t] is the concrete type of symbols. *)
  type t [@@deriving compare, eq, sexp]

  (** Languages must supply a pretty-printer for their symbols. *)
  include Pretty_printer.S with type t := t

  (** They must allow fold-mapping over any string parts of the
      symbol. *)
  module OnStrings
    : Fold_map.S0 with type elt = string and type t = t
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

  (** [to_string t] converts a [t] to a string.  The string should be
      a valid assembly-level representation of [t]. *)
  val to_string : t -> string

  (** [of_string_opt t] tries to interpret [s] as a symbol.  This
     function can return [None] if the string doesn't represent a
     legal symbol in this language. *)
  val of_string_opt : string -> t option
end

(** [R_map] is the interface of a map-like structure that represents
   redirections from symbols to other symbols.

    Implementations of [S] contain implementations of [R_map]. *)
module type R_map = sig
  (** [t] is the opaque type of a redirect map. *)
  type t

  (** [sym] is the type of symbols. *)
  type sym [@@deriving sexp, eq]

  (** [r_dest] is the type of destination results used in [R_map]. *)
  type r_dest =
    | Identity       (** The map redirects this symbol to itself *)
    | MapsTo of sym  (** The map redirects this symbol here *)
  [@@deriving sexp, eq]
  ;;

  module Set : Set.S with type Elt.t = sym

  (** [make s] creates a redirect map with the initial source
      set [s].  Each symbol is registered as mapping to itself. *)
  val make : Set.t -> t

  (** [redirect ~src ~dst rmap] marks [src] as having redirected to
     [dst] in [rmap].  It is safe for [src] and [dst] to be equal: in
     such cases we register that a redirection happened, but otherwise
     don't do anything.

      [redirect] fails if [dst] is a registered source other than [src];
      this is to prevent cycles. *)
  val redirect
    :  src : sym
    -> dst : sym
    -> t
    -> t Or_error.t
  ;;

  (** [dest_of rmap src] gives the final destination of [src] as an
     [r_dest]. *)
  val dest_of : t -> sym -> r_dest option

  (** [sources_of rmap dst] gives all of the symbols that map to
      [dst] in [rmap]. *)
  val sources_of : t -> sym -> sym list
end

(** [S] is an expanded interface onto an act language's symbol
    analysis. *)
module type S = sig
  include Basic

  (** [Set] is an extended set module for concrete symbols. *)
  module Set : sig
    include Set.S with type Elt.t = t
    include My_set.Extensions with type t := t

    (** [abstract set] applies [abstract] to every symbol in
        the concrete symbol set [set]. *)
    val abstract : t -> Abstract.Symbol.Set.t
  end

  (** [R_map] is an implementation of [R_map] for this symbol type. *)
  module R_map : R_map with type sym = t and module Set = Set

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

(** [Language_symbol] is the interface exposed in the main mli
   file. *)
module type Language_symbol = sig
  module type Basic = Basic
  module type S = S

  (** [Make] produces an instance of [S] from an instance of [Basic]. *)
  module Make : functor (B : Basic) -> S with type t = B.t
end
