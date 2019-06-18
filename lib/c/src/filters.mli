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

(** Filters for transforming C programs.

    This module exposes filters (in the unix I/O sense) for reading in C or
    C/Litmus programs from a file (or stdin), doing something to them, and
    outputting the results to a file (or stdout). *)

open Act_common

(** {2 Flags} *)

(** The operation to use in the filter. *)
type mode =
  | Print
      (** Pretty-print the result (useful for debugging). *)
  | Fuzz of {seed: int option; o: Output.t; config: Act_config.Fuzz.t}
      (** If the input is a C/Litmus file, fuzz it and return a mutated
          version. *)

(** {2 Filter modules} *)

module type S =
  Plumbing.Filter_types.S with type aux_i = mode and type aux_o = unit

(** Filter for dealing with 'normal' C programs. *)
module Normal_C : S

(** Filter for dealing with 'litmusified' C programs. *)
module Litmus : S

val c_module : bool -> (module S)
(** [c_module is_c] is [Normal_C] when [is_c] is true, and [Litmus]
    otherwise. *)
