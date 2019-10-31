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
  type 'spec t

  (** {2 Constructors} *)

  val make : id:Id.t -> spec:'spec -> 'spec t
  (** [make ~id ~spec] makes a {!t} with the given [id] and [spec]. *)

  (** {2 Accessors} *)

  val id : _ t -> Id.t
  (** [id s] gets the identifier of [s]. *)

  val spec : 'spec t -> 'spec
  (** [spec s] unwraps the identifier of [s]. *)
end

(** Specification tables, parametrised directly on the spec type.

    As 'proper' specification types have several operations needed for full
    use of specification sets, this module has very few available
    operations. See the [Set] module constructed on specification types for
    more useful functionality. *)
module Set : sig
  type 'spec t
  (** Opaque type of specification sets. *)

  val empty : 'spec t
  (** [empty] is the empty specification set. *)

  val of_list : 'spec With_id.t list -> 'spec t Or_error.t
  (** [of_list xs] tries to make a set from [xs]. It raises an error if [xs]
      contains duplicate IDs. *)

  val of_map : 'spec Map.M(Id).t -> 'spec t

  val partition_map :
       'spec t
    -> f:('spec With_id.t -> [`Fst of 'a | `Snd of 'b])
    -> 'a list * 'b list
  (** [partition_map specs ~f] applies a partitioning predicate [f] to the
      specifications in [specs], returning those marked [`Fst] in the first
      bucket and those marked [`Snd] in the second. *)

  module On_specs : Travesty.Traversable_types.S1 with type 'a t = 'a t
  (** We can monadically traverse the specifications in a set. *)
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
