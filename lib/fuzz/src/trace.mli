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

open Base

type t [@@deriving sexp]
(** Opaque type of traces. *)

(** {1 Building traces} *)

(** {2 Programmatically} *)

val empty : t
(** The empty trace. *)

val add :
     t
  -> action:(module Action_types.S with type Payload.t = 'r)
  -> payload:'r
  -> t
(** [add t ~action ~state] appends action [action], with name [id] and
    payload [payload], to the trace. *)

(** {2 From files} *)

include Plumbing.Loadable_types.S with type t := t
(** We can load traces (from S-expressions). *)

(** {1 Replaying traces} *)

val run :
     t
  -> Subject.Test.t
  -> resolve:(Act_common.Id.t -> (module Action_types.S) Or_error.t)
  -> Subject.Test.t State.Monad.t
(** [run t subject ~resolve] applies a trace [t] to test subject [subject],
    using [resolve] to resolve action IDs to their modules. *)
