(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Comparing two simulation outputs.

    This module contains a function, {{!run} run}, for running
    'diff'-comparisons between two simulation outputs: an 'oracle'
    (generally from a C litmus test) and a 'subject' (generally from its
    compiled assembly).

    Each run takes, as additional parameters, partial mappings between the
    (litmus-style) state variable identifiers, and (uninterpreted string)
    values, mapping from the subject back to the oracle. *)

open Base

(** A map from subject locations to oracle locations. *)
module Location_map : sig
  type t = Act_common.Litmus_id.t option Map.M(Act_common.Litmus_id).t
  (** Location maps are just partials Base-maps from litmus IDs to litmus IDs. *)

  include Plumbing.Jsonable_types.S with type t := t
  include Plumbing.Loadable_types.S with type t := t

  val reflexive
    : Set.M(Act_common.Litmus_id).t ->
    t
  (** [reflexive vars] makes a reflexive location map (one that maps
     each item in [vars] to itself). *)

  val output : t -> onto:Plumbing.Output.t -> unit Or_error.t
  (** [output vars ~onto] serialises [vars] to JSON and outputs it as
     instructed by [onto].  It fails if [onto] doesn't point to a valid
     output. *)
end

(** Synonym of [Set_partial_order] for orderings between state sets. *)
module Order : sig
  type t = (State.t, State.comparator_witness) Act_utils.Set_partial_order.t
end

type t =
  | Oracle_undefined
      (** The oracle execution triggered undefined behaviour. *)
  | Subject_undefined
      (** The subject execution triggered undefined behaviour. *)
  | Result of Order.t  (** Analysis completed with the following result. *)

val to_string : t -> string
(** [to_string result] returns a human-readable string representing
    [result]. *)

include
  Pretty_printer.S with type t := t
(** We can also pretty-print diff results, with similar results to running
    [to_string]. *)

val run :
     oracle:Output.Observation.t
  -> subject:Output.Observation.t
  -> location_map:Location_map.t
  -> t Or_error.t
(** [run ~oracle ~subject ~location_map] applies the partial map
    [location_map] to every state binding in [subject], then analyses it
    against [oracle]. *)
