(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstract data types for simulation output. *)

open Base

module Observation : sig
  type t [@@deriving sexp_of, quickcheck, yojson]
  (** Opaque type of a simulator observation record. *)

  include
    Plumbing.Loadable_types.S with type t := t
  (** Observations can be loaded from JSON files. *)

  (** {2 Constructing an observation record} *)

  val init : unit -> t
  (** [init ()] creates an initial observation record, with no states and no
      undefined behaviour flag. *)

  val add : t -> state:State.t -> t Or_error.t
  (** [add out ~state] adds [state] onto the state set of [out]. It fails if
      [out] is marked as having undefined behaviour. *)

  val set_undefined : t -> t Or_error.t
  (** [set_undefined out] marks [out] with an undefined behaviour flag. It
      fails if [out] is already marked as having undefined behaviour, or has
      states attached. *)

  (** {2 Accessors} *)

  val states : t -> State.t list
  (** [states out] gets the states of a single simulator output [out]. *)

  val is_undefined : t -> bool
  (** [is_undefined out] gets whether [out]'s run encountered undefined
      behaviour. *)
end

type t =
  | Success of Observation.t
  | Skipped of {why: Info.t}
  | Errored of {err: Error.t}
[@@deriving sexp_of]

(** {2 Convenience constructors} *)

val join : t Or_error.t -> t

val of_observation_or_error : Observation.t Or_error.t -> t

val not_found : Fpath.t -> t

val to_observation_or_error :
  t -> handle_skipped:[`Error | `Ignore] -> Observation.t Or_error.t
