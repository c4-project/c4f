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

(** Top-level language modules for x86 *)

open Lib

(** [S] is the signature of language modules over the X86 AST. *)
module type S = sig
  include Dialect.S
  include Pp.Printer
  include
    Language.S
    with type Constant.t = Ast.Operand.t
     and type Location.t = Ast.Location.t
     and type Instruction.t = Ast.Instruction.t
     and type Statement.t = Ast.Statement.t
     and type Program.t = Ast.t
     and type Symbol.t = string

  (** [make_jump_operand jsym] expands a jump symbol [jsym] to the
      correct abstract syntax for this version of x86. *)
  val make_jump_operand : string -> Ast.Operand.t
end

(** [Att] is a language description for the AT&T dialect of x86. *)
module Att : S

(** [Intel] is a language description for the Intel dialect of x86. *)
module Intel : S

(** [Herd7] is a language description for the Herd7 dialect of x86. *)
module Herd7 : S

(** [of_dialect] gets the correct [S] module for a dialect. *)
val of_dialect : Dialect.t -> (module S)
