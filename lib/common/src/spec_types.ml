open Base

(** [Common] contains the signature common both to plain specification
    modules ([Basic]) and the with-ID forms ([S_with_id]). *)
module type Common = sig
  (** [t] is the opaque type of specifications. *)
  type t [@@deriving equal]

  val is_enabled : t -> bool
  (** [is_enabled spec] returns true if [spec] is enabled. *)

  include Pretty_printer.S with type t := t

  val pp_summary : t Fmt.t
  (** [pp_summary f spec] prints a one-line summary of [spec]. *)
end

(** [S_with_id] is a signature for types bundling a spec ID and a type. *)
module type S_with_id = sig
  (** [elt] is the type of elements inside the bundle. *)
  type elt

  (** [t] is the opaque type of specification-ID bundles. *)
  type t

  include Common with type t := t

  val make : id:Id.t -> spec:elt -> t
  (** [make ~id ~spec] creates a new [With_id.t] pair. *)

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

  val type_name : string

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
    (** Opaque (here, at least) type of compiler spec sets. This gets fixed
        through a type alias in {{!Spec} Spec}. *)
    type t [@@deriving equal]

    include Pretty_printer.S with type t := t

    val pp_verbose : bool -> t Fmt.t
    (** [pp_verbose verbose f specs] prints a [specs] with the level of
        verbosity implied by [verbose]. *)

    val get : t -> Id.t -> With_id.t Or_error.t
    (** [get specs id] tries to look up ID [id] in [specs], and emits an
        error if it can't. *)

    val get_using_fqid : t -> fqid:Id.t -> With_id.t Or_error.t
    (** [get_using_fqid specs ~fqid] tries to look up a spec in [specs]
        whose ID forms a prefix of the 'fully qualified' ID ~fqid. *)

    val of_list : With_id.t list -> t Or_error.t
    (** [of_list xs] tries to make a set from [xs]. It raises an error if
        [xs] contains duplicate IDs. *)

    val restrict : t -> identifiers:Id.Set.t -> t
    (** [restrict specs ~identifiers] returns the map corresponding to
        [specs], but with all specs removed whose ID is not in [ids]. *)

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

  val pp_verbose : bool -> t Fmt.t
  (** [pp_verbose verbose f spec] prints a [spec] with the level of
      verbosity implied by [verbose]. *)
end
