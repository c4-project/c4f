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

open Core_kernel

(** Interface of testing timing sets. *)
module type S = sig
  (** Opaque type of the timing set. *)
  type t

  (** Type of timestamp-carrying input containers for this timing set. The
      wrapped input itself is never used. *)
  type 'a input

  (** {2 Constructors} *)

  val make :
       ?delitmusifier:_ input
    -> ?c_simulator:_ input
    -> ?compiler:_ input
    -> ?litmusifier:_ input
    -> ?asm_simulator:_ input
    -> unit
    -> t

  (** {2 Accessors} *)

  val in_delitmusifier : t -> Time.Span.t option
  (** [in_delitmusifier ts] gets the span of time spent in the
      delitmusifying stage of the tester process, according to [ts]. *)

  val in_c_simulator : t -> Time.Span.t option
  (** [in_c_simulator ts] gets the span of time spent in the C simulation
      stage of the tester process, according to [ts]. *)

  val in_compiler : t -> Time.Span.t option
  (** [in_compiler ts] gets the span of time spent in the C compilation
      stage of the tester process, according to [ts]. *)

  val in_litmusifier : t -> Time.Span.t option
  (** [in_litmusifier ts] gets the span of time spent in the litmusifying
      stage of the tester process, according to [ts]. *)

  val in_asm_simulator : t -> Time.Span.t option
  (** [in_delitmusifier ts] gets the span of time spent in the assembly
      simulation stage of the tester process, according to [ts]. *)
end
