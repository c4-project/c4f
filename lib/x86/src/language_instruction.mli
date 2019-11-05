(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** x86 language: instruction analysis

    This is the part of the x86 language definition that implements opcode
    and operand abstraction, as well as various instruction-level
    constructors. *)

(** [Basic] collects all of the prerequisite modules needed to make the
    [Instruction] part of an x86 [Language.S] module. *)
module type Basic = sig
  module Dialect : Dialect_intf.S

  module Pretty : Pp_intf.S

  module Symbol : Act_language.Symbol_types.Basic with type t := string

  module Location :
    Act_language.Location_types.Basic with type t := Ast.Location.t
end

(** [S] is the signature of a module produced using [Make]. *)
module type S = sig
  include
    Act_language.Instruction_types.Basic
      with type t = Ast.Instruction.t
       and type con = Ast.Operand.t
       and type Sym.t = string
       and type Loc.t = Ast.Location.t

  val make_jump_operand : string -> Ast.Operand.t
  (** [make_jump_operand sym] turns [sym] into a jump operand.

      This is, at time of writing, part of [Language.Basic] rather than being
      part of the instruction setup, but this may change in future. *)
end

(** [Make (B)] generates a [S] from a [Basic] [B]. *)
module Make (B : Basic) : S
