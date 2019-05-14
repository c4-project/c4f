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

open Core_kernel

(** Signature of [Partial_order] modules in extended sets. *)
module type S_partial_order = sig
  (** Type of sets being ordered *)
  type set

  type t [@@deriving sexp_of]

  (** {2 Partial conversion to total orderings} *)

  val to_ordering_opt : t -> Ordering.t option
  (** [to_ordering_opt po] converts [po] to a total ordering if it has one,
      or return [None] otherwise. *)

  val is_equal : t -> bool
  (** [is_equal po] is true if [po] represents set equality. *)

  val is_proper_superset : t -> bool
  (** [is_proper_superset po] is true if [po] represents a proper superset. *)

  val is_superset : t -> bool
  (** [is_superset po] is true if [po] represents a superset (including
      equality). *)

  val is_proper_subset : t -> bool
  (** [is_proper_subset po] is true if [po] represents a proper subset. *)

  val is_subset : t -> bool
  (** [is_subset po] is true if [po] represents a subset (including
      equality). *)

  (** {2 Extended queries} *)

  val is_unordered : t -> bool
  (** [is_unordered po] is true if [po] doesn't represent set equality,
      subset, or superset. *)

  val left_has_uniques : t -> bool
  (** [left_has_uniques po] is true if the LHS set [po] concerns has values
      not in the RHS set (that is, it's a superset or non-order). *)

  val right_has_uniques : t -> bool
  (** [left_has_uniques po] is true if the RHS set [po] concerns has values
      not in the LHS set (that is, it's a subset or non-order). *)

  val in_left_only : t -> set
  (** [in_left_only po] gets the set of all items in the LHS set [po]
      concerns, but not the RHS set. *)

  val in_right_only : t -> set
  (** [in_right_only po] gets the set of all items in the RHS set [po]
      concerns, but not the LHS set. *)
end

(** [Extensions] contains various extensions to implementations of [Set.S]. *)
module type Extensions = sig
  (** The type of sets that we're extending. *)
  type t

  val disjoint : t -> t -> bool
  (** [disjoint x y] returns [true] provided that [x] and [y] have no
      elements in common. *)

  (** Type of returns from [partial_compare]. *)
  module Partial_order : S_partial_order with type set := t

  val partial_compare : t -> t -> Partial_order.t
  (** [partial_compare x y] compares two sets [x] and [y] by analysing their
      symmetric difference. *)
end

(** [S] is [Set.S] extended with [Extensions]. *)
module type S = sig
  include Set.S

  include Extensions with type t := t
end
