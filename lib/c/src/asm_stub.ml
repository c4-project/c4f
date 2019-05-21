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

open Base

module Operand = struct
  type 'rhs t = {symbol: string option; constr: string; rhs: 'rhs}
  [@@deriving make]

  let pp_symbol : string option Fmt.t =
    Fmt.(option (suffix sp (brackets string)))

  let pp (type rhs) (pp_rhs : rhs Fmt.t) (f : Formatter.t) (op : rhs t) :
      unit =
    Fmt.(
      pf f "@[%a%s@ %a]" pp_symbol op.symbol op.constr (parens pp_rhs)
        op.rhs)
end

type t =
  { clobbers: string list
  ; input_operands: Ast.Expr.t Operand.t list
  ; output_operands: Ast.Expr.t Operand.t list
  ; template: string list }
[@@deriving make, fields]

let pp_line (pp : 'a Fmt.t) : 'a Fmt.t =
  Fmt.(hbox (prefix (unit ":@ ") (suffix sp pp)))

let pp_operands : Ast.Expr.t Operand.t list Fmt.t =
  Fmt.(list ~sep:comma (Operand.pp Ast.Expr.pp))

let pp_clobbers : string list Fmt.t = Fmt.(list ~sep:comma string)

let pp_inner (f : Formatter.t) (stub : t) : unit =
  let has_clobbers = not (List.is_empty (clobbers stub)) in
  let has_inputs =
    has_clobbers || not (List.is_empty (input_operands stub))
  in
  Fmt.(list ~sep:cut string) f (template stub) ;
  pp_line pp_operands f (output_operands stub) ;
  if has_inputs then pp_line pp_operands f (input_operands stub) ;
  if has_clobbers then pp_line pp_clobbers f (clobbers stub)

let pp : t Fmt.t =
  Fmt.(vbox ~indent:2 (prefix (unit "asm volatile") (parens pp_inner)))
