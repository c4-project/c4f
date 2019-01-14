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

open Core

module Hook
    (L : Language.S) (P : Travesty.Traversable.S1_container) = struct
  open Ast

  module Lang = L
  module Ctx = Lib.Sanitiser_ctx.Make (Lang)
  module Pass = Lib.Sanitiser_pass
  module Program_container = P

  let negate = function
    | Disp.Numeric k -> Operand.Immediate (Disp.Numeric (-k))
    | Symbolic s ->
      Operand.Bop ( Operand.Immediate (Numeric 0)
                  , Bop.Minus
                  , Operand.Immediate (Symbolic s)
                  )

  let sub_to_add_ops : Operand.t list -> Operand.t list option =
    L.bind_src_dst
      ~f:(function
          | {src = Operand.Immediate s; dst} -> Some {src = negate s; dst}
          | _ -> None)

  let sub_to_add = function
    | { Instruction.prefix; opcode = Opcode.Basic `Sub; operands} as op ->
      Option.value_map
        ~default:op
        ~f:(fun ops' -> { prefix ; opcode = Basic `Add; operands = ops' })
        (sub_to_add_ops operands)
    | { prefix; opcode = Sized (`Sub, s); operands} as op ->
      Option.value_map
        ~default:op
        ~f:(fun ops' -> { prefix ; opcode = Sized (`Add, s); operands = ops' })
        (sub_to_add_ops operands)
    | x -> x

  let drop_unsupported_length_of_long = function
      (* TODO(@MattWindsor91): ideally, we should be checking to see if
         the operands are compatible with dropping the l. *)
    | `Cmp
    | `Cmpxchg
    | `Xchg
    | `Xor as o -> Opcode.Basic (o :> Opcode.Basic.t)
    (* Opcodes below this line *shouldn't* have their lengths dropped. *)
    | `Call
    | `Ret
    (* TODO(@MattWindsor91): some of these might also need dropping. *)
    | `Add | `Mov | `Pop | `Push | `Sub as o -> Sized (o, Long)
  ;;

  (** [drop_unsupported_lengths] removes long-sized length suffixes on
      instructions where herd doesn't support them. *)
  let drop_unsupported_lengths = function
    | { Instruction.opcode = Sized (o, Long); _ } as op ->
      { op with opcode = (drop_unsupported_length_of_long o) }
    | op -> op

  let name_of_segment_offset_heap_loc segment offset program_name =
    sprintf "t%sg%sd%d"
      program_name
      (Ast.Reg.to_string segment)
      offset
  ;;

  let make_segment_offset_heap_loc segment offset =
    Ctx.(
      get_prog_name
      >>| name_of_segment_offset_heap_loc segment offset
      >>= make_fresh_heap_loc
    )
  ;;

  let segment_offset_to_heap_of_indirect i =
      let open Ctx.Let_syntax in
      match Ast.Indirect.seg i, Ast.Indirect.disp i with
      | Some s, Some (Disp.Numeric k) ->
        let%bind l  = make_segment_offset_heap_loc s k in
        let%map  l' = Ctx.add_symbol l Abstract.Symbol.Sort.Heap in
        L.Location.make_heap_loc l'
      | _ -> Ctx.return (Location.Indirect i)
  ;;

  (** [segment_offset_to_heap loc] is a contextual computation that,
      heuristically, converts memory addresses like [GS:20] to symbolic
      heap locations.

      It assumes, perhaps incorrectly, that these segments aren't
      moved, or shared per thread. *)
  let segment_offset_to_heap = function
    | Location.Indirect i -> segment_offset_to_heap_of_indirect i
    | Reg _ as l          -> Ctx.return l
  ;;

  (** [warn_unsupported_registers reg] warns if [reg] isn't
      likely to be understood by Herd. *)
  let warn_unsupported_registers
    : Ast.Reg.t -> Ast.Reg.t Ctx.t = function
    | #Ast.Reg.seg
    | #Ast.Reg.gp8
    | #Ast.Reg.gp16
    | #Ast.Reg.sp16 as reg ->
      Ctx.(
        warn
          (Warn.Location (Location.Reg reg))
          (Info.of_string
             "This register is unlikely to be supported by Herd")
        >>| fun () -> reg
      )
    | #Ast.Reg.gp32
    | #Ast.Reg.sp32
    | #Ast.Reg.flag as reg -> Ctx.return reg
  ;;

  let on_register reg =
    Ctx.(
      return reg
      >>= (`Warn |-> warn_unsupported_registers)
    )
  ;;

  let through_all_registers loc =
    let module F = Location.On_registers.On_monad (Ctx) in
    F.map_m ~f:on_register loc
  ;;

  let on_statement = Ctx.return
  let on_program = Ctx.return
  let on_all = Ctx.return

  let on_location loc =
    Ctx.(
      return loc
      >>= segment_offset_to_heap
      >>= through_all_registers
    )
  ;;

  let on_instruction stm =
    Ctx.(
      return stm
      >>| sub_to_add
      >>| drop_unsupported_lengths
    )
  ;;
end

module Make_single (L : Language.S) = Lib.Sanitiser.Make_single (Hook (L))
module Make_multi  (L : Language.S) = Lib.Sanitiser.Make_multi  (Hook (L))
