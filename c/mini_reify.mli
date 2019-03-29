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

(** Functions for reifying a mini-model into an AST. *)

val func : Mini.Identifier.t -> Mini.Function.t -> Ast.External_decl.t
(** [func id f] reifies the mini-function [f], with name [id], into the C
    AST. *)

val program : Mini.Program.t -> Ast.Translation_unit.t
(** [program p] reifies the mini-program [p] into the C AST. *)

val decl : Mini.Identifier.t -> Mini.Initialiser.t -> Ast.Decl.t
(** [decl id d] reifies the mini-declaration [d], with name [id], into the C
    AST. *)

val stm : Mini.Statement.t -> Ast.Stm.t
(** [stm s] reifies the mini-statement [s] into the C AST. *)
