(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Action traces.

    This module describes a (de)serialisable log of the various actions that
    occurred during a fuzzing run. *)

open Base

(** Opaque type of traces. *)
type t [@@deriving sexp]

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

val take : t -> int -> t
(** [take t n] truncates [t] to its first [n] items. *)

(** {2 To and from files} *)

(** We can load traces (from S-expressions). *)
include Plumbing.Loadable_types.S with type t := t

(** We can store traces (to S-expressions). *)
include Plumbing.Storable_types.S with type t := t

(** {1 Replaying traces} *)

val run :
     t
  -> Subject.Test.t
  -> resolve:(Act_common.Id.t -> (module Action_types.S) Or_error.t)
  -> Subject.Test.t State.Monad.t
(** [run t subject ~resolve] applies a trace [t] to test subject [subject],
    using [resolve] to resolve action IDs to their modules. *)

(** {1 Investigating traces} *)

val length : t -> int
(** [length t] gets the number of actions in [t]. *)

val bisect :
     t
  -> want:[`Last_on_left | `First_on_right]
  -> f:(t -> [`Left | `Right])
  -> t
(** [bisect t ~want ~f] reduces [t] by binary search, repeatedly invoking [f]
    on various truncations of [t] as a heuristic. Depending on the value of
    [want], it will return the rightmost trace marked 'left' or the leftmost
    trace marked 'right', where 'left' means 'towards the empty trace' and
    'right' means 'towards [t]'. *)
