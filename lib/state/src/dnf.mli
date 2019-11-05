(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Converting state observations to disjunctive postconditions

    This module contains functions and filter plumbing for taking a set of
    witness states and generating a disjunctive-normal-form for-all Litmus
    postcondition (or optimisation thereof) that requires all future states
    to be one of the original witnesses. *)

open Base

(** {1 Conversion functions} *)

val predicate_of_state : Entry.t -> string Act_litmus.Predicate.t
(** [predicate_of_state entry] converts [entry] into a conjunction predicate. *)

val predicate_of_states : Set.M(Entry).t -> string Act_litmus.Predicate.t
(** [predicate_of_states entries] converts the set [entries] into a sum of
    products predicate. *)

val convert_states : Set.M(Entry).t -> string Act_litmus.Postcondition.t
(** [convert_states entries] produces a for-all Litmus postcondition that
    requires a state to be one of the states in [entries]. *)

val convert : Observation.t -> string Act_litmus.Postcondition.t
(** [convert obs] is {!convert_states} on the witnesses of [obs]. *)

(** {1 As a filter} *)

(** Wraps {!convert} up as a filter that parses an observation JSON file,
    then emits a Litmus postcondition. *)
module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit
