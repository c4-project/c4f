(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Pretty-printers for Litmus tests *)

include module type of Pp_intf
(** As usual, we store the signatures for [Pp] in a separate
   implementation module, and include them in both sides of this
   module. *)

module Make_tabular (Ast : Ast.S) : S with module Ast = Ast
(** [Make_tabular] makes a pretty-printer for a Litmus AST that
    outputs programs as tables.

    This is useful for assembly languages. *)

module Make_sequential (Ast : Ast.S) : S with module Ast = Ast
(** [Make_sequential] makes a pretty-printer for a Litmus AST that
    directly outputs each program AST in sequence.

    This is useful for languages like C. *)
