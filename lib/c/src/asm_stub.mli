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

(** Syntax and support for GCC 'asm' directives. *)

open Base

module Operand : sig
  (** Opaque type of operands, parametrised over their right-hand side. *)
  type 'rhs t

  val make : ?symbol:string -> constr:string -> rhs:'rhs -> unit -> 'rhs t
  (** [make ?symbol constr rhs ()] makes a GCC asm operand with the
      constraint [constr], the right hand side [rhs] (a C lvalue for output
      operands, and a C expression for inputs), and optional symbolic
      reference [symbol]. *)
end

(** Opaque type of asm directives. *)
type t

val make :
     ?clobbers:string list
  -> ?input_operands:Ast.Expr.t Operand.t list
  -> ?output_operands:Ast.Expr.t Operand.t list
  -> ?template:string list
  -> unit
  -> t
(** [make ?clobbers ?input_operands ?output_operands ?template ()] builds a
    GCC asm directive with template body [template], operands
    [input_operands] and [output_operands], and clobbers [clobbers]. *)

include Pretty_printer.S with type t := t
