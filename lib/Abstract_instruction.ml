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

open Base
open Stdio
open Utils

module M = struct
  type t =
    | Arith
    | Call
    | Compare
    | Fence
    | Jump
    | Logical
    | Move
    | Nop
    | Return
    | Rmw
    | Stack
    | Other
    | Unknown
  [@@deriving enum, sexp]
  ;;

  let table =
    [ Arith  , "arith"
    ; Call   , "call"
    ; Compare, "compare"
    ; Fence  , "fence"
    ; Jump   , "jump"
    ; Logical, "logical"
    ; Move   , "move"
    ; Nop    , "nop"
    ; Return , "return"
    ; Rmw    , "RMW"
    ; Stack  , "stack"
    ; Other  , "other"
    ; Unknown, "??"
    ]
end

include M
include Enum.Extend_table (M)

type with_operands =
  { opcode : t
  ; operands : Abstract_operands.t
  }
[@@deriving fields, make, sexp]
;;

module Flag = Abstract_flag.None

module type S_properties = sig
  type t

  val is_jump : t -> bool
  val is_symbolic_jump : t -> bool
  val is_symbolic_jump_where
    : t -> f:(Abstract_symbol.t -> bool) -> bool
  ;;
  val is_nop : t -> bool
  val is_stack_manipulation : t -> bool
end

module Properties : S_properties with type t := with_operands = struct
  let is_jump { opcode; _ } = equal opcode Jump

  let is_symbolic_jump_target_where operands ~f =
    match operands with
    | `Single (`Symbol sym)
    | `Single (`Location (Abstract_location.Heap sym)) -> f sym
    | _ -> false
  ;;

  let is_symbolic_jump_where { opcode; operands } ~f =
    equal opcode Jump && is_symbolic_jump_target_where operands ~f
  ;;
  let is_symbolic_jump = is_symbolic_jump_where ~f:(Fn.const true)


  let%expect_test "is_symbolic_jump: seemingly unconditional jump" =
    let result =
      is_symbolic_jump
          (make_with_operands ~opcode:Jump ~operands:`None)
    in
    Out_channel.printf "%b" result;
    [%expect {| false |}]
  ;;

  let%expect_test "is_symbolic_jump: jump to immediate symbol" =
    let result =
      is_symbolic_jump
          (make_with_operands
            ~opcode:Jump
            ~operands:(`Single (`Symbol "foo")))
    in
    Out_channel.printf "%b" result;
    [%expect {| true |}]
  ;;

  let%expect_test "is_symbolic_jump: jump to heap symbol" =
    let result =
      is_symbolic_jump
          (make_with_operands
            ~opcode:Jump
            ~operands:(`Single
                         (`Location (Abstract_location.Heap "foo"))))
    in
    Out_channel.printf "%b" result;
    [%expect {| true |}]
  ;;

  let is_nop { opcode; _ } = equal opcode Nop

  let is_stack_manipulation { opcode; operands } = match opcode with
    | Stack -> true
    | Arith -> Abstract_operands.has_stack_pointer_dst operands
    | Move  -> Abstract_operands.(
        has_stack_pointer_src operands || has_stack_pointer_dst operands
      )
    | _     -> false
  ;;
end
include Properties

module Forward_properties
    (F : sig
       include S_properties
       type fwd
       val forward : fwd -> t
     end) : S_properties with type t := F.fwd = struct
  let is_jump x = F.is_jump (F.forward x)
  let is_nop x = F.is_nop (F.forward x)
  let is_stack_manipulation x = F.is_stack_manipulation (F.forward x)
  let is_symbolic_jump_where x ~f = F.is_symbolic_jump_where (F.forward x) ~f
  let is_symbolic_jump x = F.is_symbolic_jump (F.forward x)
end
