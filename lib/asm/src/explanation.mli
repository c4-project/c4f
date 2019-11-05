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

(** High-level explainer output. *)

open Explanation_intf

(** [Make (B)] makes an [S] from a [Basic]. *)
module Make (B : Basic) :
  S
    with type elt := B.elt
     and type context := B.context
     and type details := B.details
     and module Abs := B.Abs
     and module Flag := B.Flag

(** {2 Specific explanations} *)

module Make_loc (L : Act_language.Location_types.S) :
  S
    with type elt := L.t
     and type context := Act_abstract.Symbol.Table.t
     and module Abs := Act_abstract.Location

module Make_ops (L : Act_language.Instruction_types.S) :
  S
    with type elt := L.t
     and type context := Act_abstract.Symbol.Table.t
     and module Abs := Act_abstract.Operand.Bundle

module Make_ins (B : Basic_ins) :
  S
    with type elt := B.Instruction.t
     and type context := Act_abstract.Symbol.Table.t
     and module Abs := Act_abstract.Instruction

module Make_stm (B : Basic_stm) : sig
  include
    S
      with type elt := B.Statement.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Statement

  val instructions : details -> B.Ins_expl.t list
end
