(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Set extensions *)

open Core_kernel

(** Type of returns from [partial_compare],
    where ['a] is the set type under comparison. *)
module Partial_order : sig
  type 'a t =
    | Equal (** Both sets are equal. *)
    | Subset of { in_right_only : 'a } (** RHS has the following extra values. *)
    | Superset of { in_left_only : 'a } (** LHS has the following extra values. *)
    | No_order of { in_left_only : 'a; in_right_only : 'a }
        (** LHS and RHS both have the following extra values. *)
  [@@deriving sexp]
end

(** [Extensions] contains various extensions to implementations of
   [Set.S]. *)
module type Extensions = sig
  (** The type of sets that we're extending. *)
  type t

  (** [disjoint x y] returns [true] provided that [x] and [y] have no
      elements in common. *)
  val disjoint : t -> t -> bool

  (** [partial_compare x y] compares two sets [x] and [y] by analysing
      their symmetric difference. *)
  val partial_compare : t -> t -> t Partial_order.t
end

(** [Extend] builds set extensions for module [S]. *)
module Extend (S : Set.S) : Extensions with type t := S.t
