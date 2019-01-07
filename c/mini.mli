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

(** A miniature model of a memalloy-witness-style C program.

    Unlike {{!Ast}Ast}, which tries to be a fairly faithful C abstract
    syntax tree, this module describes a tiny subset of C that
    maps well to litmus tests, and does so in a fairly high-level
    manner.

    One can get a 'mini' C program by {{!Convert}converting} an AST
    to it (which may fail).  To get an AST (for printing, etc.), use
    {{!Reify}Reify}.
 *)

open Base

module Type : sig
  type t
end

module Initialiser : sig
  type t
end

module Expression : sig
  type t [@@deriving sexp]

  val constant : Ast_basic.Constant.t -> t
  (** [constant k] lifts a C constant [k] to an expression. *)
end

module Statement : sig
  type t [@@deriving sexp]

  val assign : lvalue:Ast_basic.Identifier.t -> rvalue:Expression.t -> t
  (** [assign ~lvalue ~rvalue] lifts a C assignment to an expression. *)
end

module Function : sig
  type t
end

module Program : sig
  type t
end

(** Reifying a mini-model into an AST *)
module Reify : sig
  val func : Ast_basic.Identifier.t -> Function.t -> Ast.External_decl.t

  val program : Program.t -> Ast.Translation_unit.t
end

(** Converting an AST into the mini-model *)
module Convert : sig
  val func
    :  Ast.Function_def.t
    -> (Ast_basic.Identifier.t * Function.t) Or_error.t
  ;;

  val translation_unit : Ast.Translation_unit.t -> Program.t Or_error.t
end
