(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A single state in a simulator run output.

    The only thing stopping this module from being called [State] is the
    fact that [State] is already the name of the {i library}. *)

open Base
open Act_common

type t [@@deriving sexp, compare, yojson, quickcheck]
(** [t] is the type of states: a binding from name to value. *)

include Comparable.S with type t := t

include Pretty_printer.S with type t := t

val map :
     location_map:(Litmus_id.t -> Litmus_id.t option Or_error.t)
  -> value_map:(string -> string Or_error.t)
  -> t
  -> t Or_error.t
(** [map ~location_map ~value_map t] maps partial mappers over the keys and
    values of state [t]. [location_map] may return [Ok None] if the key
    should be deleted in the new map.

    If all invocations of [location_map] and [value_map] return values, and
    the result is a well-formed map [m], [map] returns [Ok m]; else, an
    error. *)

val of_alist : (Litmus_id.t, string) List.Assoc.t -> t Or_error.t
(** [of_alist alist] tries to convert [alist] into a state. *)

(** {2 Domain queries} *)

val domain : t -> Set.M(Litmus_id).t
(** [domain t] gets the list of all bound names in [t]. *)

val common_domain : t list -> Set.M(Litmus_id).t Or_error.t
(** [common_domain t] gets the list of all bound names across [ts] if the
    states in [ts] agree on a single such list, or an inconsistency error
    otherwise.*)

val restrict : t -> domain:Set.M(Litmus_id).t -> t
(** [restrict t ~domain] removes all mappings in [t] that don't reference
    identifiers in [domain]. *)

val maps_to : t -> key:Litmus_id.t -> data:string -> bool
(** [maps_to t ~key ~data] is [true] if, and only if, [t] contains a mapping
    from [key] to [data]. *)

(** The specific set type for entries, combined with several pre-derived
    instances for use in quickchecking and serialising things that contain
    entry sets. *)
module Set : sig
  type nonrec t = (t, comparator_witness) Set.t

  include Plumbing.Jsonable_types.S with type t := t

  include Sexpable.S with type t := t

  include Act_utils.My_quickcheck.S_with_sexp with type t := t
end
