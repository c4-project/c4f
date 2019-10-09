(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** This module mostly contains tests for the similarly named source module
    (not exposed in this signature), but also has various helpers for
    building tests for observations. *)

open Base

module Test_utils : sig
  (** {2 Non-robust shorthands for building observations quickly} *)

  val add_entries_exn :
       ?tag:Act_state.Observation.Entry_tag.t
    -> (string, string) List.Assoc.t list
    -> Act_state.Observation.t
    -> Act_state.Observation.t
  (** [add_entries_exn ?tag entries obs] converts [entries] to an entry set
      using [Entry.Test_utils.entries_exn], then adds them to [obs] while
      raising for any errors from doing so. *)
end
