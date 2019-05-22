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
module Ac = Act_common
module Tx = Travesty_base_exts

let make_template_indirect (ind : Ast.Indirect.t) : string =
  ignore ind ; "TODO"

let make_template_location : Ast.Location.t -> Ast.Location.t = function
  | Reg r ->
      Ast.Location.Template_token (Reg.to_string r)
  | Indirect i ->
      Ast.Location.Template_token (make_template_indirect i)
  | Template_token _ as tok ->
      tok

let make_template_instruction : Ast.Instruction.t -> Ast.Instruction.t =
  Ast.Instruction.On_locations.map ~f:make_template_location

let make_template_statement : Ast.Statement.t -> Ast.Statement.t =
  Ast.Statement.On_instructions.map ~f:make_template_instruction

let make_template (asm : Ast.t) : string list =
  let asm' = Ast.On_statements.map asm ~f:make_template_statement in
  let statements = Ast.On_statements.to_list asm' in
  List.map ~f:(Fmt.strf "@[%a@]@." Pp.Att.pp_statement) statements

let require_att (asm : Ast.t) : unit Or_error.t =
  Tx.Or_error.unless_m
    (Ac.Id.has_tag (Ast.dialect asm) "att")
    ~f:(fun () ->
      Or_error.error_s
        [%message
          "Stub generation requires AT&T dialect assembly input."
            ~actual_dialect:(Ast.dialect asm : Ac.Id.t)] )

let run (asm : Ast.t) : Act_c.Asm_stub.t Or_error.t =
  Or_error.Let_syntax.(
    let%map () = require_att asm in
    let template = make_template asm in
    Act_c.Asm_stub.make ~template ())
