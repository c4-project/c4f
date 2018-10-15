(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core

module Hook (L : Language.Intf) =
struct
  open Ast

  module L = L
  module Ctx = Lib.Sanitiser.CtxMake (L) (Lib.Sanitiser.NoCustomWarn)

  let negate = function
    | DispNumeric k -> OperandImmediate (DispNumeric (-k))
    | DispSymbolic s -> OperandBop ( OperandImmediate (DispNumeric 0)
                                   , BopMinus
                                   , OperandImmediate (DispSymbolic s)
                                   )

  let sub_to_add_ops : operand list -> operand list option =
    L.bind_src_dst
      ~f:(function
          | {src = OperandImmediate s; dst} -> Some {src = negate s; dst}
          | _ -> None)

  let sub_to_add =
    function
    | { prefix; opcode = OpBasic `Sub; operands} as op ->
      Option.value_map
        ~default:op
        ~f:(fun ops' -> { prefix ; opcode = OpBasic `Add; operands = ops' })
        (sub_to_add_ops operands)
    | { prefix; opcode = OpSized (`Sub, s); operands} as op ->
      Option.value_map
        ~default:op
        ~f:(fun ops' -> { prefix ; opcode = OpSized (`Add, s); operands = ops' })
        (sub_to_add_ops operands)
    | x -> x

  (** [drop_unsupported_lengths] removes long-sized length suffixes on
      instructions where herd doesn't support them. *)
  let drop_unsupported_lengths =
    (* TODO(@MattWindsor91): ideally, we should be checking to see if
       the operands are compatible with dropping the l. *)
    function
    | { opcode = OpSized (o, SLong); _ } as op ->
      begin
        match o with
        | `Cmp -> { op with opcode = OpBasic (o :> basic_opcode) }
        | `Ret
        (* TODO(@MattWindsor91): some of these might also need dropping. *)
        | `Add | `Mov | `Pop | `Push | `Sub -> op
      end
    | op -> op

  let on_statement = Ctx.return

  let on_program = Ctx.return

  let on_location = Ctx.return

  let on_instruction stm =
    let open Ctx in
    return stm
    >>| sub_to_add
    >>| drop_unsupported_lengths
end

module Make (L : Language.Intf) = Lib.Sanitiser.Make (Hook (L))
