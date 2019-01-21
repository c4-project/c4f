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

(** Converting an AST into the mini-model.

    This module contains partial functions that try to convert a full
    C AST into the mini-model.  They fail if the AST contains pieces of
    C syntax that aren't expressible in the mini-model.
 *)

open Base

val func
  :  Ast.Function_def.t
  -> (Mini.Identifier.t * Mini.Function.t) Or_error.t
(** [func ast] tries to interpret a C function definition AST
    as a mini-model function. *)

val translation_unit : Ast.Translation_unit.t -> Mini.Program.t Or_error.t
(** [translation_unit ast] tries to interpret a C translation unit AST
    as a mini-model program. *)

val litmus : Ast.Litmus.Validated.t -> Mini.Litmus_ast.Validated.t Or_error.t
(** [litmus test] tries to interpret a Litmus test over the full C AST
    as one over the mini-model. *)
