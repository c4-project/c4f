(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A generalised variable map with a notion of scoped access. *)

open Base

type 'a t [@@deriving equal]
(** Opaque type of scoped maps. *)

(** {1 Constructing a scoped map} *)

val empty : 'a t
(** [empty] is the empty scoped map. *)

val of_litmus_id_map : 'a Map.M(Litmus_id).t -> 'a t
(** [of_litmus_id_map map] constructs a scoped map from a map [map] from
    Litmus-style scoped identifiers to records. *)

val of_litmus_id_alist : (Litmus_id.t, 'a) List.Assoc.t -> 'a t Or_error.t
(** [of_litmus_id_alist xs] constructs a scoped map from an associative list
    [xs] from Litmus-style scoped identifiers to records. It fails if [xs]
    has duplicate keys. *)

(** {1 Looking up variable identifiers} *)

val find_by_litmus_id : 'a t -> id:Litmus_id.t -> 'a Or_error.t
(** [find_by_litmus_id map ~id] directly looks up a litmus ID in [map]
    without any scope resolution. It returns an error if [id] isn't in [map]. *)

val resolve : _ t -> id:C_id.t -> scope:Scope.t -> Litmus_id.t
(** [resolve map ~id ~scope] returns the thread-local version of [id] against
    [scope] if such a version exists in the variable map, or the global
    version of [id] otherwise. It doesn't always check whether [id] is in the
    variable map. *)

(** {2 Transforming scoped maps} *)

val set : 'a t -> id:Litmus_id.t -> record:'a -> 'a t
(** [set map ~id ~record] returns a new map produced by setting [id]'s record
    to [record]. *)

val map_record : 'a t -> f:('a -> 'a) -> id:Litmus_id.t -> 'a t
(** [map_record map ~f ~id] returns a new map produced by mapping [f] over
    the record of variable [id], if present. *)

val filter : 'a t -> f:('a -> bool) -> 'a t
(** [filter map ~f] filters [map] according to [f]. *)

(** {2 Projections of scoped maps} *)

val build_set :
     (module Comparable.S with type t = 'e and type comparator_witness = 'w)
  -> 'a t
  -> f:(Litmus_id.t -> 'a -> 'e option)
  -> ('e, 'w) Set.t
(** [build_set (Carrier) map ~f] is a general function for projecting a
    scoped map down to a set. *)

(** {3 Projections to maps} *)

val to_litmus_id_map : 'a t -> 'a Map.M(Litmus_id).t
(** [to_litmus_id_map map] converts [map] to a flat map from Litmus-style IDs
    to records. *)

val to_c_id_map : 'a t -> scope:Scope.t -> 'a Map.M(C_id).t
(** [to_c_id_map map ~scope] projects [map] down to the map of variables
    accessible through scope [scope]. Local variables shadow global
    variables. *)

val c_id_mem : _ t -> id:C_id.t -> bool
(** [cid_mem map ~id] returns whether there exists a mapping in [map] from a
    variable with C identifier [id], regardless of scope. *)

(** {2 Serialising scoped maps} *)

(** [Make_json] makes a Yojson serialiser-deserialiser pair for a scoped map,
    using the given module for serialising records *)
module Make_json (Record : Plumbing.Jsonable_types.S) :
  Plumbing.Jsonable_types.S with type t = Record.t t
