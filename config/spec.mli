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

(** [Spec] contains general interfaces for dealing with specifications of
    machines and compilers. *)

open Core

(** [Common] contains the signature common both to plain specification
    modules ([Basic]) and the with-ID forms ([S_with_id]). *)
module type Common = sig
  (** [t] is the opaque type of specifications. *)
  type t [@@deriving sexp]

  val is_enabled : t -> bool
  (** [is_enabled spec] returns true if [spec] is enabled. *)

  include Pretty_printer.S with type t := t

  val pp_summary : Format.formatter -> t -> unit
  (** [pp_summary f spec] prints a one-line summary of [spec]. *)
end

(** [S_with_id] is a signature for types bundling a spec ID and a type. *)
module type S_with_id = sig
  (** [elt] is the type of elements inside the bundle. *)
  type elt

  (** [t] is the opaque type of specification-ID bundles. *)
  type t

  include Common with type t := t

  val create : id:Id.t -> spec:elt -> t
  (** [create ~id ~spec] creates a new [With_id.t] pair. *)

  val id : t -> Id.t
  (** [id w] gets the ID component of a [w]. *)

  val spec : t -> elt
  (** [spec w] gets the spec component of [w]. *)
end

(** [Basic] is the basic interface of both compiler and machine
    specifications. *)
module type Basic = sig
  (** [t] is the opaque type of specifications. *)
  type t

  include Common with type t := t

  (** [With_id] contains types and functions for handling bundles of spec ID
      and spec. *)
  module With_id : S_with_id with type elt := t
end

(** [S] is the top-level, outward-facing interface of both compiler and
    machine specifications. *)
module type S = sig
  include Basic

  (** [Set] is the interface of modules for dealing with sets of compiler
      specs. *)
  module Set : sig
    (** [t] is the type of sets. *)
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    val pp_verbose : bool -> Format.formatter -> t -> unit
    (** [pp_verbose verbose f specs] prints a [specs] with the level of
        verbosity implied by [verbose]. *)

    val get : t -> Id.t -> With_id.t Or_error.t
    (** [get specs id] tries to look up ID [id] in [specs], and emits an
        error if it can't. *)

    val of_list : With_id.t list -> t Or_error.t
    (** [of_list xs] tries to make a set from [xs]. It raises an error if
        [xs] contains duplicate IDs. *)

    val restrict : t -> Id.Set.t -> t
    (** [restrict specs ids] returns the map corresponding to [specs], but
        with all specs removed whose ID is not in [ids]. *)

    val partition_map :
      t -> f:(With_id.t -> [`Fst of 'a | `Snd of 'b]) -> 'a list * 'b list
    (** [partition_map specs ~f] applies a partitioning predicate [f] to the
        specifications in [specs], returning those marked [`Fst] in the
        first bucket and those marked [`Snd] in the second. *)

    val group : t -> f:(With_id.t -> Id.t) -> t Id.Map.t
    (** [group specs ~f] groups [specs] into buckets according to some
        grouping function [f]. [f] returns specification IDs; the idea is
        that this allows grouping of specifications by references to other,
        larger specifications. *)

    val map : t -> f:(With_id.t -> 'a) -> 'a list
    (** [map specs ~f] applies a mapper [f] to the specifications in
        [specs], returning the results as a list. *)
  end

  val pp_verbose : bool -> Format.formatter -> t -> unit
  (** [pp_verbose verbose f spec] prints a [spec] with the level of
      verbosity implied by [verbose]. *)
end

(** [With_id] is a basic implementation of [S_with_id] for specs with type
    [B.t].

    Usually, spec modules should extend [With_id] to implement the various
    accessors they expose on the spec type itself, for convenience. *)
module With_id (C : Common) : S_with_id with type elt := C.t

(** [Make] makes an [S] from a [Basic]. *)
module Make (B : Basic) :
  S with type t := B.t and module With_id := B.With_id
