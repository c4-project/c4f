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

(** Set extensions *)

open Core_kernel

(** ['a partial_order] is the type of returns from [partial_compare],
    where ['a] is the set type under comparison. *)
type 'a partial_order =
  [ `Equal
  | `Subset   of 'a (** RHS has the following extra values. *)
  | `Superset of 'a (** LHS has the following extra values. *)
  | `NoOrder
  ] [@@deriving sexp]
;;

(** [Extensions] contains various extensions to implementations of
   [Set.S]. *)
module type Extensions = sig
  type t
  (** [disjoint x y] returns [true] provided that [x] and [y] have no
      elements in common. *)
  val disjoint : t -> t -> bool

  (** [partial_compare x y] compares two sets [x] and [y] by analysing
      their symmetric difference. *)
  val partial_compare : t -> t -> t partial_order
end

(** [Extend] builds set extensions for module [S]. *)
module Extend : functor (S : Set.S) -> Extensions with type t := S.t;;
