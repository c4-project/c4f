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

(** [Spec] contains general interfaces for dealing with specifications
    of machines and compilers. *)

open Core

(** [Id] is a module for compiler and machine identifiers. *)
module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t

  (** [to_string_list cid] returns a list of each element in [cid]'s
     ID. *)
  val to_string_list : t -> string list

  include Identifiable.S with type t := t
end

(** [Basic] is the basic interface of both compiler and machine
   specifications. *)
module type Basic = sig
  (** [t] is the opaque type of specifications.
      To construct a [t], read one in as an S-expression;
      a proper constructor may appear in later revisions. *)
  type t [@@deriving sexp]

  (** [enabled c] gets whether [c] is enabled. *)
  val enabled : t -> bool

  include Pretty_printer.S with type t := t

  (** [pp_summary f spec] prints a one-line summary of [spec]. *)
  val pp_summary : Format.formatter -> t -> unit
end

(** [S] is the top-level, outward-facing interface of both
   compiler and machine specifications. *)
module type S = sig
  include Basic

  (** [With_id] contains types and functions for handling bundles of
     spec ID and spec. *)
  module With_id : sig
    type elt = t

    (** [t] is the type of ID-and-spec pairs. *)
    type t [@@deriving sexp]

    (** [create ~id ~spec] creates a new [With_id.t] pair. *)
    val create : id:Id.t -> spec:elt -> t

    (** [id w] gets the ID component of a [w]. *)
    val id : t -> Id.t

    (** [spec w] gets the spec component of [w]. *)
    val spec : t -> elt

    (** [to_tuple w] gets the ID and spec components of [w] as a
       tuple. *)
    val to_tuple : t -> (Id.t * elt)
  end

  (** [Set] is the interface of modules for dealing with sets of
      compiler specs. *)
  module Set : sig
    type elt = t

    (** [t] is the type of sets. *)
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    (** [pp_verbose verbose f specs] prints a [specs] with the level of
        verbosity implied by [verbose]. *)
    val pp_verbose : bool -> Format.formatter -> t -> unit

    (** [get specs id] tries to look up ID [id] in [specs],
        and emits an error if it can't. *)
    val get : t -> Id.t -> elt Or_error.t

    (** [of_list xs] tries to make a set from [xs].
        It raises an error if [xs] contains duplicate IDs. *)
    val of_list : With_id.t list -> t Or_error.t

    (** [partition_map specs ~f] applies a partitioning predicate [f] to the
        specifications in [specs], returning those marked [`Fst] in
        the first bucket and those marked [`Snd] in the second. *)
    val partition_map
      :  t
      -> f : (With_id.t -> [`Fst of 'a | `Snd of 'b])
      -> ('a list * 'b list)
    ;;

    (** [map specs ~f] applies a mapper [f] to the
        specifications in [specs], returning the results as a list. *)
    val map
      :  t
      -> f : (With_id.t -> 'a)
      -> 'a list
    ;;
  end

  (** [pp_verbose verbose f spec] prints a [spec] with the level of
      verbosity implied by [verbose]. *)
  val pp_verbose : bool -> Format.formatter -> t -> unit
end

(** [Make] makes an [S] from a [Basic]. *)
module Make : functor (B : Basic) -> S with type t = B.t

