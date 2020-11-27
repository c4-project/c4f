(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** This module mostly contains tests for the similarly named source module
    (not exposed in this signature), but also has various helpers for
    building tests for entries. *)

open Base

module Test_utils : sig
  (** {2 Non-robust shorthands for building entries quickly} *)

  val entry_exn : (string, string) List.Assoc.t -> Act_state.Entry.t
  (** [entry_exn xs] makes an entry from a string-string list, throwing an
      exception if this fails. *)

  val entries_exn :
    (string, string) List.Assoc.t list -> Set.M(Act_state.Entry).t
  (** [entries_exn xs] makes an entry set from a string-string list-list,
      throwing an exception if this fails. *)
end
