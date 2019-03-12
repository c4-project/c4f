(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** De-litmusification of memalloy-style C litmus tests *)

open Base
open Utils

(** The output from a de-litmusification round. *)
module Output : sig
  type t
  (** Opaque type of de-litmusification output. *)

  val program : t -> Mini.Program.t
  (** [program output] gets the de-litmusified program. *)

  val c_globals : t -> C_identifier.Set.t
  (** [c_globals output] gets the names of each global variable
      contained in the de-litmusified program. *)

  val c_locals : t -> C_identifier.Set.t
  (** [c_locals output] gets the names of each 'local' variable
      contained in the de-litmusified program.
      (We actually model them as global variables, but they serve
      to capture what were local variables in the original test.) *)
end


val run : Mini_litmus.Ast.Validated.t -> Output.t Or_error.t
(** [run litmus] runs de-litmusification on [litmus]. *)
