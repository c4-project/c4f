(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

module Observation = struct
  module M = struct
    type t = {states: State.t list; is_undefined: bool}
    [@@deriving fields, sexp_of, quickcheck, yojson]
  end
  include M
  include Plumbing.Loadable.Of_jsonable (M)

  (** [init ()] generates an initial [t]. *)
  let init () = {states= []; is_undefined= false}

  let add (out : t) ~(state : State.t) : t Or_error.t =
    match out with
    | {is_undefined= true; _} ->
        Or_error.error_s
          [%message
            "Can't add state to simulation output, as the output is marked \
             undefined"
              ~state:(state : State.t)]
    | {is_undefined= false; states} ->
        Or_error.return {is_undefined= false; states= state :: states}

  let set_undefined : t -> t Or_error.t = function
    | {is_undefined= true; _} ->
        Or_error.error_string
          "Simulation output already marked as undefined"
    | {states= []; _} ->
        Or_error.return {states= []; is_undefined= true}
    | {states; _} ->
        Or_error.error_s
          [%message
            "Can't mark simulation output as undefined, as it has states"
              ~states:(states : State.t list)]
end

type t =
  | Success of Observation.t
  | Skipped of {why: Info.t}
  | Errored of {err: Error.t}
[@@deriving sexp_of]

let join : t Or_error.t -> t = function
  | Ok success ->
      success
  | Error err ->
      Errored {err}

let of_observation_or_error (o : Observation.t Or_error.t) : t =
  join (Or_error.map ~f:(fun x -> Success x) o)

let not_found (litmus_path : Fpath.t) : t =
  let path = Fpath.to_string litmus_path in
  let why =
    Info.create_s [%message "Path not found in simulation cache" ~path]
  in
  Skipped {why}

let to_observation_or_error (o : t) ~(handle_skipped : [`Error | `Ignore]) :
    Observation.t Or_error.t =
  match (o, handle_skipped) with
  | Success obs, _ ->
      Or_error.return obs
  | Errored {err}, _ ->
      Result.Error err
  | Skipped {why}, `Error ->
      Result.Error (Error.of_info why)
  | Skipped {why= _}, `Ignore ->
      Or_error.return (Observation.init ())
