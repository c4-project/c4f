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

let qstring : string Fmt.t = Fmt.(quote string)

module Operand = struct
  type 'rhs t = {symbol: string option; constr: string; rhs: 'rhs}
  [@@deriving make]

  let pp_symbol : string option Fmt.t =
    Fmt.(option (suffix sp (brackets string)))

  let pp (type rhs) (pp_rhs : rhs Fmt.t) (f : Formatter.t) (op : rhs t) :
      unit =
    Fmt.(
      pf f "@[%a%a@ %a@]" pp_symbol op.symbol qstring op.constr
        (parens pp_rhs) op.rhs)
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

let pp_clobbers : string list Fmt.t = Fmt.(list ~sep:comma qstring)

let tabify_string (s : string) : string = s ^ "\\n\\t"

let tabify_template (template : string list) : string list =
  let last = List.length template - 1 in
  List.mapi template ~f:(fun (k : int) (str : string) ->
      let str' = String.strip str in
      if Int.equal k last then str' else tabify_string str' )

let pp_template : string list Fmt.t =
  Fmt.(using tabify_template (vbox (list ~sep:sp (quote string))))

let pp_inner (f : Formatter.t) (stub : t) : unit =
  let has_clobbers = not (List.is_empty (clobbers stub)) in
  let has_inputs =
    has_clobbers || not (List.is_empty (input_operands stub))
  in
  pp_template f (template stub) ;
  Fmt.sp f () ;
  pp_line pp_operands f (output_operands stub) ;
  Fmt.sp f () ;
  if has_inputs then (
    pp_line pp_operands f (input_operands stub) ;
    Fmt.sp f () ) ;
  if has_clobbers then pp_line pp_clobbers f (clobbers stub)

let pp : t Fmt.t =
  Fmt.(
    box ~indent:2
      (prefix (unit "asm volatile@ ")
         (suffix (unit ";") (parens (vbox pp_inner)))))
