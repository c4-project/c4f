(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functor for building Herdtools7 output scrapers.

    Both Herd7 and Litmus7 have similar, but not quite identical, output.
    This module provides {{!Make} a functor} that accepts the bits that are
    different, and fills in the common ground. *)

(** Abstract type of semi-parsed Herdtools7 state lines.

    The common reader logic makes much use of this structure internally, but
    also relies on the tool-specific parser sending it a semi-parsed state
    line using the same structure. *)
module State_line : sig
  type 'a t
  (** Opaque type of state lines. The type parameter refers to the part of
      the line containing the state entries, which the common reader logic
      parses; usually it'll be [string] outside of the module boundary. *)

  val make :
    ?occurrences:int -> ?tag:Act_state.Observation.Entry_tag.t -> 'a -> 'a t
  (** [make ?occurrences ?tag rest] makes a state line with the optional
      already-parsed occurrence number [occurrences], entry tag [tag], and
      the string containing the part of the state line that is common to
      both Herd and Litmus. *)
end

module type Basic =
  Reader_intf.Basic with type state_line := string State_line.t
(** Synonym for {{!Reader_intf.Basic} Reader_intf.Basic} with [state_line]
    fixed. *)

module Make (B : Basic) : Act_backend.Reader_types.S
(** Makes an output scraper for the Herdtools7 simulator described in [B]. *)
