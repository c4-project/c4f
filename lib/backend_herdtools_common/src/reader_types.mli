(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures and types for building Herdtools output scrapers. *)

open Base

(** Signature over the tool-specific parts of a Herdtools output scraper. *)
module type Basic = sig
  (** Filled in with {!Reader.Test_type}. *)
  type test_type

  (** Filled in with {!Reader.State_line}. *)
  type state_line

  val try_parse_state_count : string -> int option
  (** [try_parse_state_count line] should return [Some k] if preamble line
      [line] contains a state count with [k] states, and [None] otherwise.
      (It needn't do any further validation.) *)

  val try_split_state_line : test_type -> string -> state_line Or_error.t
  (** [try_split_state_line test_type line] should try to split [line] into
      the state tag (taking into account [test_type], the (optional) number
      of occurrences of that state, and an otherwise unparsed line containing
      the state data. *)
end
