(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [Spec] contains general interfaces for dealing with specifications of
    machines and compilers. *)

open Base

module With_id : sig
  type 'spec t [@@deriving equal]

  (** {2 Constructors} *)

  val make : id:Id.t -> spec:'spec -> 'spec t
  (** [make ~id ~spec] makes a {!t} with the given [id] and [spec]. *)

  (** {2 Accessors} *)

  val id : _ t -> Id.t
  (** [id s] gets the identifier of [s]. *)

  val spec : 'spec t -> 'spec
  (** [spec s] unwraps the identifier of [s]. *)

  (** {2 Traversals} *)

  (** We can traverse over the specification inside a with-ID pair. *)
  include Travesty.Traversable_types.S1 with type 'spec t := 'spec t
end

(** Specification tables, parametrised directly on the spec type.

    As 'proper' specification types have several operations needed for full
    use of specification sets, this module has very few available operations.
    See the [Set] module constructed on specification types for more useful
    functionality. *)
module Set : sig
  (** Opaque type of specification sets. *)
  type 'spec t [@@deriving equal, sexp]

  val empty : 'spec t
  (** [empty] is the empty specification set. *)

  val of_list : 'spec With_id.t list -> 'spec t Or_error.t
  (** [of_list xs] tries to make a set from [xs]. It raises an error if [xs]
      contains duplicate IDs. *)

  val to_list : 'spec t -> 'spec With_id.t list
  (** [to_list xs] is the inverse of {!of_list}. *)

  val of_map : 'spec Map.M(Id).t -> 'spec t

  val get : ?id_type:string -> 'spec t -> id:Id.t -> 'spec Or_error.t
  (** [get ?id_type specs ~id] tries to look up ID [id] in [specs], and emits
      an error if it can't. If [id_type] is given, it will appear as the ID
      type in such errors. *)

  val get_with_fqid :
       ?id_type:string
    -> 'spec t
    -> prefixes:Id.t list
    -> fqid:Id.t
    -> 'spec Or_error.t
  (** [get_with_fqid ?id_type specs ~prefixes ~fqid] behaves as {!get}, but,
      on lookup error, tries again with each prefix in [prefix] added to
      [fqid] in turn. This is intended to model searches by a fully qualified
      ID, where [prefixes] contains defaults for the qualifying part of the
      ID. *)

  (** {2 Projections} *)

  val map : 'spec t -> f:('spec With_id.t -> 'a) -> 'a list
  (** [map specs ~f] applies a mapper [f] to the specifications in [specs],
      returning the results as a list. *)

  val partition_map :
    'spec t -> f:('spec With_id.t -> ('a, 'b) Either.t) -> 'a list * 'b list
  (** [partition_map specs ~f] applies a partitioning predicate [f] to the
      specifications in [specs], returning those marked [`Fst] in the first
      bucket and those marked [`Snd] in the second. *)

  (** We can monadically traverse the specifications in a set. *)
  module On_specs : Travesty.Traversable_types.S1 with type 'a t = 'a t
end

module type S = sig
  type t

  include Spec_types.S with type Set.t = t Set.t and type t := t
end

(** [With_id] is a basic implementation of [S_with_id] for specs with type
    [B.t].

    Usually, spec modules should extend [With_id] to implement the various
    accessors they expose on the spec type itself, for convenience. *)
module Make_with_id (C : Spec_types.Common) :
  Spec_types.S_with_id with type elt = C.t and type t = C.t With_id.t

(** [Make] makes an [S] from a [Basic]. *)
module Make (B : Spec_types.Basic) :
  S with type t = B.t and module With_id = B.With_id

(** {1 Helpers for specification modules} *)

val pp_enabled_summary : bool Fmt.t
(** [pp_enabled_summary] pretty-prints a Boolean representing an
    enabled/disabled flag on a spec. It emits "enabled" if true, and
    "disabled" otherwise, potentially with styling. *)
