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

(** Signatures and types for building Herdtools output scrapers. *)

open Base

(** Signature over the tool-specific parts of a Herdtools output scraper. *)
module type Basic = sig
  type state_line

  val try_parse_state_count : string -> int option
  (** [try_parse_state_count line] should return [Some k] if preamble line
      [line] contains a state count with [k] states, and [None] otherwise.
      (It needn't do any further validation.) *)

  val try_split_state_line : string -> state_line Or_error.t
  (** [try_split_state_line line] should try to split a state line into the
      (optional) number of occurrences of that state, and an otherwise
      unparsed line containing the state data. *)
end
