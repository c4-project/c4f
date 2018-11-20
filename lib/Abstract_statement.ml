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

type t =
  | Directive of string
  | Instruction of Abstract_instruction.with_operands
  | Blank
  | Label of Abstract_symbol.t
  | Other
[@@deriving sexp, variants]

let pp f = function
  | Blank -> ()
  | Directive d -> Format.fprintf f "directive@ (%s)" d
  | Label l -> Format.fprintf f ":%s"             l
  | Instruction ins ->
    Format.fprintf f "@[%a@ %a@]"
      Abstract_instruction.pp (Abstract_instruction.opcode ins)
      Abstract_operands.pp (Abstract_instruction.operands ins)
  | Other -> String.pp f "??"
;;

module Flag = struct
  module M = struct
    type t =
      [ `UnusedLabel
      | `StackManip
      ] [@@deriving enum, enumerate, sexp]

    let table =
      [ `UnusedLabel, "unused label"
      ; `StackManip, "manipulates stack"
      ]
  end

  include M
  include Enum.Extend_table (M)
end

(** [S_properties] is the signature of any module that can access
    properties of an abstract statement. *)
module type S_properties = sig
  type t
  include Abstract_instruction.S_properties with type t := t
  val is_directive : t -> bool
  val is_instruction : t -> bool
  val is_instruction_where
    :  t
    -> f:(Abstract_instruction.with_operands -> bool)
    -> bool
  ;;
  val is_label : t -> bool
  val is_label_where
    :  t
    -> f:(Abstract_symbol.t -> bool)
    -> bool
  ;;
  val is_unused_label
    : t
    -> symbol_table:Abstract_symbol.Table.t
    -> bool
  ;;
  val is_jump_pair : t -> t -> bool
  val is_blank : t -> bool
  val flags : t -> Abstract_symbol.Table.t -> Flag.Set.t
end

module Properties : S_properties with type t := t = struct
  let exists
      ?(directive = Fn.const false)
      ?(instruction = Fn.const false)
      ?(label = Fn.const false)
      ?(blank = false)
      ?(other = false)
      stm =
    Variants.map stm
      ~directive:(Fn.const directive)
      ~instruction:(Fn.const instruction)
      ~label:(Fn.const label)
      ~blank:(Fn.const blank)
      ~other:(Fn.const other)
  ;;

  let is_directive stm = exists ~directive:(Fn.const true) stm
  let is_blank stm = exists ~blank:true stm

  let is_label_where stm ~f = exists ~label:f stm
  let is_label = is_label_where ~f:(Fn.const true)
  let is_unused_label stm ~symbol_table =
    is_label_where stm ~f:(fun label ->
        Abstract_symbol.(
          not (Table.mem symbol_table label ~sort:Sort.Jump)
        )
      )
  ;;

  let is_instruction_where stm ~f = exists ~instruction:f stm
  let is_instruction = is_instruction_where ~f:(Fn.const true)

  let is_jump =
    is_instruction_where ~f:Abstract_instruction.is_jump
  ;;
  let is_symbolic_jump_where t ~f =
    is_instruction_where t
      ~f:(Abstract_instruction.is_symbolic_jump_where ~f)
  ;;

  let is_stack_manipulation =
    is_instruction_where ~f:Abstract_instruction.is_stack_manipulation
  ;;

  let is_nop stm =
    is_instruction_where ~f:Abstract_instruction.is_nop stm
  ;;

  let is_jump_pair j l =
    is_symbolic_jump_where j
      ~f:(fun j_sym ->
          is_label_where l ~f:(Abstract_symbol.equal j_sym)
        )
  ;;

  let flags stm symbol_table =
    [ is_unused_label ~symbol_table stm, `UnusedLabel
    ; is_stack_manipulation stm, `StackManip
    ]
    |> List.filter_map ~f:(Tuple2.uncurry Option.some_if)
    |> Flag.Set.of_list
  ;;
end
include Properties

module Forward_properties
    (F : sig
       include S_properties
       type fwd
       val forward : fwd -> t
     end) : S_properties with type t := F.fwd = struct
  let is_directive x = F.is_directive (F.forward x)
  let is_blank x = F.is_blank (F.forward x)
  let is_instruction x = F.is_instruction (F.forward x)
  let is_instruction_where x ~f = F.is_instruction_where (F.forward x) ~f
  let is_label x = F.is_label (F.forward x)
  let is_unused_label x = F.is_unused_label (F.forward x)
  let is_label_where x ~f = F.is_label_where (F.forward x) ~f
  let is_nop x = F.is_nop (F.forward x)
  let is_stack_manipulation x = F.is_stack_manipulation (F.forward x)
  let is_jump x = F.is_jump (F.forward x)
  let is_symbolic_jump_where x ~f = F.is_symbolic_jump_where (F.forward x) ~f
  let is_jump_pair = My_fn.on F.forward F.is_jump_pair
  let flags x = F.flags (F.forward x)
end
