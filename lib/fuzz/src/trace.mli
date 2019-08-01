(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Action traces.

    This module describes a (de)serialisable log of the various actions that
    occurred during a fuzzing run. *)

type t [@@deriving sexp]
(** Opaque type of traces. *)

(** {2 Building traces} *)

val empty : t
(** The empty trace. *)

val add :
     t
  -> action:(module Action_types.S with type Payload.t = 'r)
  -> payload:'r
  -> t
(** [add t ~action ~state] appends action [action], with name [id] and
    payload [payload], to the trace. *)
