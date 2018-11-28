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

open Core_kernel
open Utils

module Opcode = struct
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

  (** For now, instructions form their own kind enumeration.
      This may change if we add more information to abstract instructions
      later. *)
  module Kind = struct
    include M
    include Enum.Extend_table (M)
  end

  let kind = Fn.id
  let pp = Kind.pp

  module Flag = Abstract_flag.None
end

type t =
  { opcode : Opcode.t
  ; operands : Abstract_operand.Bundle.t
  }
[@@deriving fields, make, sexp]
;;

module Kind = Opcode.Kind
let kind { opcode; _ } = Opcode.kind opcode

let pp f ins =
  Format.fprintf f "@[<hv>%a@ (@,%a@,)@]"
    Opcode.Kind.pp (opcode ins)
    Abstract_operand.Bundle.pp (operands ins)
;;

module Flag = Abstract_flag.None

module type S_predicates = sig
  type t

  val has_opcode : t -> opcode:Opcode.Kind.t -> bool
  val opcode_in : t -> opcodes:Opcode.Kind.Set.t -> bool

  val is_jump : t -> bool
  val is_symbolic_jump : t -> bool
  val is_symbolic_jump_where
    : t -> f:(Abstract_symbol.t -> bool) -> bool
  ;;
  val is_nop : t -> bool
  val is_stack_manipulation : t -> bool
end

module Inherit_predicates
    (P : S_predicates) (I : Utils.Inherit.S_partial with type c := P.t)
  : S_predicates with type t := I.t = struct
  open Option
  let has_opcode x ~opcode =
    exists ~f:(P.has_opcode ~opcode) (I.component_opt x)
  ;;
  let opcode_in x ~opcodes =
    exists ~f:(P.opcode_in ~opcodes) (I.component_opt x)
  ;;
  let is_jump x = exists ~f:P.is_jump (I.component_opt x)
  let is_nop x = exists ~f:P.is_nop (I.component_opt x)
  let is_stack_manipulation x = exists ~f:P.is_stack_manipulation (I.component_opt x)
  let is_symbolic_jump_where x ~f =
    exists ~f:(P.is_symbolic_jump_where ~f) (I.component_opt x)
  ;;
  let is_symbolic_jump x = exists ~f:P.is_symbolic_jump (I.component_opt x)
end

module type S_properties = sig
  type t
  include S_predicates with type t := t

  val opcode : t -> Opcode.t
  val operands : t -> Abstract_operand.Bundle.t
end

module Properties : S_properties with type t := t = struct
  let opcode = opcode
  let operands = operands

  let has_opcode { opcode=actual; _ } ~opcode =
    Opcode.Kind.equal opcode actual
  ;;

  let opcode_in { opcode=actual; _ } ~opcodes =
    Opcode.Kind.Set.mem opcodes actual
  ;;

  let is_jump = has_opcode ~opcode:Jump

  let is_symbolic_jump_where { opcode; operands } ~f =
    Opcode.Kind.equal opcode Jump
    && Abstract_operand.Bundle.is_single_jump_symbol_where operands ~f
  ;;
  let is_symbolic_jump = is_symbolic_jump_where ~f:(Fn.const true)


  let%expect_test "is_symbolic_jump: seemingly unconditional jump" =
    let result =
      is_symbolic_jump
          (make ~opcode:Jump ~operands:Abstract_operand.Bundle.None)
    in
    Out_channel.printf "%b" result;
    [%expect {| false |}]
  ;;

  let%expect_test "is_symbolic_jump: jump to immediate symbol" =
    let result =
      is_symbolic_jump
          (make
            ~opcode:Jump
            ~operands:Abstract_operand.(Bundle.Single (Symbol "foo")))
    in
    Out_channel.printf "%b" result;
    [%expect {| true |}]
  ;;

  let%expect_test "is_symbolic_jump: jump to heap symbol" =
    let result =
      is_symbolic_jump
          (make
            ~opcode:Jump
            ~operands:Abstract_operand.(
                Bundle.Single
                  (Location
                     (Abstract_location.(Heap
                                           (Address.Symbol "foo"))))))
    in
    Out_channel.printf "%b" result;
    [%expect {| true |}]
  ;;

  let is_nop { opcode; _ } = Opcode.Kind.equal opcode Nop

  let is_stack_manipulation { opcode; operands } = match opcode with
    | Stack -> true
    | Arith -> Abstract_operand.(
        Bundle.has_dst_where ~f:is_stack_pointer operands
      )
    | Move  -> Abstract_operand.Bundle.has_stack_pointer operands
    | _     -> false
  ;;
end
include Properties

module Inherit_properties
    (P : S_properties)
    (I : Inherit.S with type c := P.t)
  : S_properties with type t := I.t = struct

  let opcode x = P.opcode (I.component x)
  let operands x = P.operands (I.component x)

  module I_with_c = struct
    type c = P.t
    include I
  end
  include Inherit_predicates (P) (Utils.Inherit.Make_partial (I_with_c))

end
