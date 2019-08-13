(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Functor for building Herdtools7 output scrapers.

    Both Herd7 and Litmus7 have similar, but not quite identical, output.
    This module provides {{!Make} a functor} that accepts the bits that are
    different, and fills in the common ground. *)

module State_line : sig
  type t
  (** Opaque type of state lines. *)

  val make :
       ?occurrences:int
    -> ?tag:Act_state.Observation.Entry_tag.t
    -> string
    -> t
  (** [make ?occurrences ?tag rest] makes a state line with the optional
      already-parsed occurrence number [occurrences], entry tag [tag], and
      the string containing the part of the state line that is common to
      both Herd and Litmus. *)
end

module type Basic = Reader_intf.Basic with type state_line := State_line.t
(** Synonym for {{!Reader_intf.Basic} Reader_intf.Basic} with [state_line]
    fixed. *)

module Make (B : Basic) : Act_backend.Reader_types.S
(** Makes an output scraper for the Herdtools7 simulator described in [B]. *)
