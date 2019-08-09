(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  | Success of Act_state.Observation.t
  | Skipped of {why: Info.t}
  | Errored of {err: Error.t}
[@@deriving sexp_of]

let join : t Or_error.t -> t = function
  | Ok success ->
      success
  | Error err ->
      Errored {err}

let of_observation_or_error (o : Act_state.Observation.t Or_error.t) : t =
  join (Or_error.map ~f:(fun x -> Success x) o)

let not_found (litmus_path : Fpath.t) : t =
  let path = Fpath.to_string litmus_path in
  let why =
    Info.create_s [%message "Path not found in simulation cache" ~path]
  in
  Skipped {why}

let to_observation_or_error (o : t) ~(handle_skipped : [`Error | `Ignore]) :
    Act_state.Observation.t Or_error.t =
  match (o, handle_skipped) with
  | Success obs, _ ->
      Or_error.return obs
  | Errored {err}, _ ->
      Result.Error err
  | Skipped {why}, `Error ->
      Result.Error (Error.of_info why)
  | Skipped {why= _}, `Ignore ->
      Or_error.return Act_state.Observation.empty
