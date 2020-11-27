(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A map from subject locations to oracle locations. *)

open Base

(** Location maps are just partial Base-maps from litmus IDs to litmus IDs. *)
type t = Act_common.Litmus_id.t option Map.M(Act_common.Litmus_id).t

include Plumbing.Jsonable_types.S with type t := t

include Plumbing.Loadable_types.S with type t := t

val reflexive : Set.M(Act_common.Litmus_id).t -> t
(** [reflexive vars] makes a reflexive location map (one that maps each item
    in [vars] to itself). *)

val output : t -> onto:Plumbing.Output.t -> unit Or_error.t
(** [output vars ~onto] serialises [vars] to JSON and outputs it as
    instructed by [onto]. It fails if [onto] doesn't point to a valid output. *)
