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

(** De-litmusification of memalloy-style C litmus tests *)

open Base

(** The output from a de-litmusification round. *)
module Output : sig
  (** Opaque type of de-litmusification output. *)
  type t

  val program : t -> Mini.Program.t
  (** [program output] gets the de-litmusified program. *)

  val c_variables : t -> Config.C_variables.Map.t
  (** [c_variables output] gets a map containing information about each
      global and local variable contained in the de-litmusified program. *)
end

val run : Mini_litmus.Ast.Validated.t -> Output.t Or_error.t
(** [run litmus] runs de-litmusification on [litmus]. *)
