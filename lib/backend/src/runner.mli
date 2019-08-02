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

(** Litmus simulation: runner interfaces.

    This module provides a standard interface for interacting with C and
    assembly simulators, such as Herd and Litmus, as well as a functor for
    implementing that interface using a filter wrapper over the simulator
    and a simulator output parser. *)

open Base

module Make (B : sig
  module Reader : Reader_types.S

  module Unchecked_filter : Filter.S

  val make_harness_unchecked :
       Arch.t
    -> input_path:Fpath.t
    -> output_dir:Fpath.t
    -> string list Or_error.t
end) : Runner_types.S
(** [Make] makes a simulator runner. *)

module Make_error (B : sig
  val error : Error.t
end) : Runner_types.S

val no_make_harness :
     Arch.t
  -> input_path:Fpath.t
  -> output_dir:Fpath.t
  -> string list Or_error.t
(** [no_make_harness] is a dummy value for [make_harness] that raises an
    error stating that the backend doesn't support it. *)
