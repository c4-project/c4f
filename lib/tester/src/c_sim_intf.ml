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

open Base

(** {2 Input and output types} *)

type 'elt source =
  | From_test of {input_path: Fpath.t; output_path: Fpath.t}
      (** This simulation run will get ['elt] from the Litmus test at
          [input_path]. It may use [output_path] as an output file if needed
          (for example, to communicate with the simulator). *)
  | Inline of 'elt
      (** This simulation run will get ['elt] directly from the user. *)

(** Type of postcondition input. *)
type post = Act_c.Mini_litmus.Ast.Postcondition.t

(** Type of extensional state-set input. *)
type ext = Act_sim.State.Set.t

(** {2 Signatures} *)

module type S = sig
  val run_post : post source -> post Or_error.t

  val run_ext : ext source -> ext Or_error.t

  val run :
       [`Post of post source | `Ext of ext source]
    -> [`Post of post | `Ext of ext] Or_error.t
  (** [run input] behaves as [run_post] or [run_ext], depending on the input
      query. *)
end
