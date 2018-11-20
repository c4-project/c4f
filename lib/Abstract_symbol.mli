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

(** [Abstract_symbol] contains types and utilities for abstracted
    symbols. *)

open Core_kernel
open Utils

(** Symbols are strings. *)
type t = string [@@deriving sexp, eq]

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

  include Enum.Extension_table with type t := t
end

(** [Table] is a module concerning symbol tables: many-to-many
    mappings between symbols and sorts. *)
module Table : sig
  type elt = t
  type t

  (** [empty] is the empty table. *)
  val empty : t

  (** [of_sets sets] expands a symbol-set-to-sort associative list
      into a [t]. *)
  val of_sets : (Set.t, Sort.t) List.Assoc.t -> t

  (** [add tbl sym sort] registers [sym] as a symbol with sort
      [sort] in [tbl], returning a new table. *)
  val add : t -> elt -> Sort.t -> t

  (** [remove tbl sym sort] de-registers [sym] as a symbol with sort
      [sort] in [tbl], returning a new table.

      If [sym] also maps to another sort, those mappings remain. *)
  val remove : t -> elt -> Sort.t -> t

  (** [set_of_sorts tbl sorts] returns all symbols in [tbl] with a
      sort in [sorts], as a symbol set. *)
  val set_of_sorts : t -> Sort.Set.t -> Set.t

  (** [set_of_sort tbl sort] returns all symbols in [tbl] with sort
      [sort], as a symbol set. *)
  val set_of_sort : t -> Sort.t -> Set.t

  (** [set tbl] returns all symbols in [tbl]
      as a symbol set. *)
  val set : t -> Set.t

  (** [mem tbl ?sort symbol] checks whether [symbol] is
      in [tbl].  If [sort] is present, we additionally
      require that [symbol] has sort [sort] in [tbl]. *)
  val mem : t -> ?sort:Sort.t -> elt -> bool
end
