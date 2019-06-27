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

(** Element-wise partial order on sets. *)

open Base

type ('v, 'cmp) t
(** Opaque type of partial order results. *)

(** {2 Partial conversion to total orderings} *)

val to_ordering_opt : (_, _) t -> Ordering.t option
(** [to_ordering_opt po] converts [po] to a total ordering if it has one, or
    return [None] otherwise. *)

val is_equal : (_, _) t -> bool
(** [is_equal po] is true if [po] represents set equality. *)

val is_proper_superset : (_, _) t -> bool
(** [is_proper_superset po] is true if [po] represents a proper superset. *)

val is_superset : (_, _) t -> bool
(** [is_superset po] is true if [po] represents a superset (including
    equality). *)

val is_proper_subset : (_, _) t -> bool
(** [is_proper_subset po] is true if [po] represents a proper subset. *)

val is_subset : (_, _) t -> bool
(** [is_subset po] is true if [po] represents a subset (including equality). *)

(** {2 Extended queries} *)

val is_unordered : (_, _) t -> bool
(** [is_unordered po] is true if [po] doesn't represent set equality,
    subset, or superset. *)

val left_has_uniques : (_, _) t -> bool
(** [left_has_uniques po] is true if the LHS set [po] concerns has values
    not in the RHS set (that is, it's a superset or non-order). *)

val right_has_uniques : (_, _) t -> bool
(** [left_has_uniques po] is true if the RHS set [po] concerns has values
    not in the LHS set (that is, it's a subset or non-order). *)

val in_left_only : ('v, 'cmp) t -> ('v, 'cmp) Set.t
(** [in_left_only po] gets the set of all items in the LHS set [po]
    concerns, but not the RHS set. *)

val in_right_only : ('v, 'cmp) t -> ('v, 'cmp) Set.t
(** [in_right_only po] gets the set of all items in the RHS set [po]
    concerns, but not the LHS set. *)

val make : ('v, 'cmp) Set.t -> ('v, 'cmp) Set.t -> ('v, 'cmp) t
(** [make x y] compares two sets [x] and [y] by analysing their symmetric
    difference. *)

module M (C : sig
  type t

  type comparator_witness
end) : sig
  type nonrec t = (C.t, C.comparator_witness) t
end
