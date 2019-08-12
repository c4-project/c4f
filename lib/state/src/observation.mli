(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** An observation of some execution or simulation's states. *)

open Base

type t [@@deriving sexp_of, quickcheck, yojson]
(** Opaque type of a simulator observation record. *)

include
  Plumbing.Loadable_types.S with type t := t
(** Observations can be loaded from JSON files. *)

(** {1 Constructing an observation record} *)

val empty : t
(** [empty] is an empty observation record. *)

(** Type of tag that can be attached to entries added using {!add}. *)
module Entry_tag : sig
  type t =
    | Witness
    | Counter_example
    | Unknown
end

val add :
  ?tag:Entry_tag.t -> t -> state:Entry.t -> t Or_error.t
(** [add ?kind out ~state] adds [state] onto the state set of [out]. It
    fails if [out] is marked as having undefined behaviour. If [tag] is
    given, [state] may also propagate to the observation's witness or
    counter-example set accordingly. *)

val set_undefined : t -> t Or_error.t
(** [set_undefined out] marks [out] with an undefined behaviour flag. It
    fails if [out] is already marked as having undefined behaviour, or has
    states attached. *)

val set_unsat : t -> t Or_error.t
(** [set_unsat out] marks [out] with a flag claiming that it does not
    satisfy its original postcondition. It fails if [out] is already marked
    as being either sat or unsat. *)

val set_sat : t -> t Or_error.t
(** [set_sat out] marks [out] with a flag claiming that it satisfies its
    original postcondition. It fails if [out] is already marked as being
    either sat or unsat. *)

(** {1 Accessors} *)

(** {2 States}

    Generally, the union of {!witnesses} and {!counter_examples} will be a
    subset of {!states}. *)

val states : t -> Set.M(Entry).t
(** [states out] gets the state entries of a single observation [out]. *)

val counter_examples : t -> Set.M(Entry).t
(** [counter_examples out] gets the state entries of a single observation
    [out] that were marked as failing the postcondition. This set isn't
    guaranteed to contain all counter-examples: for example, a backend might
    simply not mark states as such. *)

val witnesses : t -> Set.M(Entry).t
(** [witnesses out] gets the state entries of a single observation [out]
    that were marked as passing the postcondition. This set isn't guaranteed
    to contain all witnesses: for example, a backend might simply not mark
    states as such. *)

(** {2 Flags}

    Note that it is possible for both {!is_unsat} and {!is_sat} to be false
    (if, for example, the run was somehow inconsistent). *)

val is_undefined : t -> bool
(** [is_undefined out] gets whether [out]'s run encountered undefined
    behaviour. *)

val is_unsat : t -> bool
(** [is_unsat out] gets whether [out]'s run did not satisfy its original
    postcondition. *)

val is_sat : t -> bool
(** [is_sat out] gets whether [out]'s run satisfied its original
    postcondition. *)
