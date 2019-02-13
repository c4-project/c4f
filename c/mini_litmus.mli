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

(** Mini-model: Litmus tests

    This module declares modules and functions for manipulating litmus
    tests over act's 'mini' subset of C. *)

open Utils

(** The mini-model, packaged up as a Litmus language.

    This language uses {{!Reify}Reify} for all of its pretty-printing
    needs. *)
module Lang : Litmus.Ast.Basic
  with type Statement.t =
         [ `Stm of Mini.Statement.t
         | `Decl of (Mini.Identifier.t * Mini.Initialiser.t)
         ]
   and type Program.t = (Mini.Identifier.t * Mini.Function.t)
   and type Constant.t = Mini.Constant.t
;;

(** The mini-model's full Litmus AST module. *)
module Ast : Litmus.Ast.S with module Lang = Lang

(** Pretty-printing for the mini-model's litmus AST. *)
module Pp : Litmus.Pp.S with module Ast = Ast

val cvars : Ast.Validated.t -> C_identifier.Set.t
(** cvars ast] gets the list of C variables referenced in a
   mini-C Litmus test. *)
