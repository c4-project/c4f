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

open Base

module Make (L : Element_types.Basic) :
  Element_types.S
    with type ins = L.Instruction.t
     and type loc = L.Location.t
     and type stm = L.Statement.t
     and type sym = L.Symbol.t = struct
  type ins = L.Instruction.t

  type loc = L.Location.t

  type stm = L.Statement.t

  type sym = L.Symbol.t

  type t =
    | Instruction of L.Instruction.t
    | Location of L.Location.t
    | Operands of L.Instruction.t
    | Statement of L.Statement.t
    | Symbol of L.Symbol.t

  let pp (f : Formatter.t) : t -> unit = function
    | Statement s ->
        L.Statement.pp f s
    | Instruction i | Operands i ->
        L.Instruction.pp f i
    | Location l ->
        L.Location.pp f l
    | Symbol s ->
        L.Symbol.pp f s

  let type_name : t -> string = function
    | Instruction _ ->
        "instruction"
    | Location _ ->
        "location"
    | Operands _ ->
        "operands of instruction"
    | Statement _ ->
        "statement"
    | Symbol _ ->
        "symbol"
end
