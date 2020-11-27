(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Record summarising the difference between two state sets. *)

open Base

(** Opaque type of partial order results. *)
type t [@@deriving yojson]

(** Diffs are pretty-printable (though the result isn't guaranteed to be
    machine-readable; use the Yojson instance for interoperability with other
    tools). *)
include Pretty_printer.S with type t := t

(** {1 Construction} *)

val make : Set.M(Entry).t -> Set.M(Entry).t -> t
(** [make x y] compares two sets [x] and [y] by analysing their symmetric
    difference. *)

(** {1 Queries on diffs} *)

(** {2 Partial conversion to total orderings} *)

val to_ordering_opt : t -> Ordering.t option
(** [to_ordering_opt po] converts [po] to a total ordering if it has one, or
    return [None] otherwise. *)

(** {2 Relational queries} *)

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
(** [is_subset po] is true if [po] represents a subset (including equality). *)

val is_unordered : t -> bool
(** [is_unordered po] is true if [po] doesn't represent set equality, subset,
    or superset. *)

val left_has_uniques : t -> bool
(** [left_has_uniques po] is true if the LHS set [po] concerns has values not
    in the RHS set (that is, it's a superset or non-order). *)

val right_has_uniques : t -> bool
(** [left_has_uniques po] is true if the RHS set [po] concerns has values not
    in the LHS set (that is, it's a subset or non-order). *)

(** {2 Extracting sets and information about them} *)

val in_left_only : t -> Set.M(Entry).t
(** [in_left_only po] gets the set of all items in the LHS set [po] concerns,
    but not the RHS set. *)

val in_right_only : t -> Set.M(Entry).t
(** [in_right_only po] gets the set of all items in the RHS set [po]
    concerns, but not the LHS set. *)
