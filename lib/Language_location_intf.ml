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

(** Language abstraction layer: location analysis

    This module contains the interface act languages must implement
   for location analysis, [Basic] and [Basic_with_modules]; the
   expanded interface the rest of act gets, [S]; and a functor from
   one to the other, [Make]. *)

open Core_kernel

(** [Basic] is the interface act languages must implement for
    location analysis. *)
module type Basic = sig
  (** [t] is the type of locations. *)
  type t [@@deriving sexp, eq]

  module Sym : Equal.S
  (** Type of concrete symbols. *)

  (** Languages must supply a pretty-printer for their locations. *)
  include Pretty_printer.S with type t := t

  (** They must allow traversal over symbols... *)
  module On_symbols :
    Travesty.Traversable.S0_container with module Elt = Sym
                                       and type t := t
  ;;

  include Abstract.Abstractable.S
    with type t := t
     and module Abs := Abstract.Location
  ;;

  (** [make_heap_loc sym] creates a location referencing a symbolic heap
      location [sym]. *)
  val make_heap_loc : Sym.t -> t
end

(** [Basic_with_modules] extends [Basic] with the fully expanded
    language abstraction layer modules on which [Make] depends. *)
module type Basic_with_modules = sig
  module Symbol : Language_symbol.S

  include Basic with module Sym := Symbol
end

(** [S] is an expanded interface onto an act language's location
    analysis. *)
module type S = sig
  include Basic_with_modules

  (** We can query abstract properties directly on the concrete
       location type. *)
  include Abstract.Location.S_predicates with type t := t
end

(** [Language_location] is the interface exposed in the main mli
   file. *)
module type Language_location = sig
  module type Basic = Basic
  module type Basic_with_modules = Basic_with_modules
  module type S = S

  (** [Make] produces an instance of [S] from an instance of
     [Basic_with_modules]. *)
  module Make (B : Basic_with_modules)
    : S with type t = B.t
         and module Symbol = B.Symbol
  ;;
end
