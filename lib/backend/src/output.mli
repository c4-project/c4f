(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstract data types for simulation output. *)

open Base

type t =
  | Success of Act_state.Observation.t
  | Skipped of {why: Info.t}
  | Errored of {err: Error.t}
[@@deriving sexp_of]

(** {2 Convenience constructors} *)

val join : t Or_error.t -> t

val of_observation_or_error : Act_state.Observation.t Or_error.t -> t

val not_found : Fpath.t -> t

val to_observation_or_error :
     t
  -> handle_skipped:[`Error | `Ignore]
  -> Act_state.Observation.t Or_error.t
