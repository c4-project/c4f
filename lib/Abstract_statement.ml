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

module type S_predicates = sig
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
  val is_label_where : t -> f:(string -> bool) -> bool
  val is_unused_label
    : t
    -> symbol_table:Abstract_symbol.Table.t
    -> bool
  ;;
  val is_jump_pair : t -> t -> bool
  val is_blank : t -> bool
end

module Inherit_predicates
    (P : S_predicates) (I : Utils.Inherit.S_partial with type c := P.t)
  : S_predicates with type t := I.t = struct
  include Abstract_instruction.Inherit_predicates
      (P)
      (struct
        type t = I.t
        let component_opt = I.component_opt
      end)

  let is_directive x =
    Option.exists ~f:P.is_directive (I.component_opt x)
  ;;
  let is_blank x = Option.exists ~f:P.is_blank (I.component_opt x)
  let is_instruction x =
    Option.exists ~f:P.is_instruction (I.component_opt x)
  ;;
  let is_instruction_where x ~f =
    Option.exists ~f:(P.is_instruction_where ~f) (I.component_opt x)
  ;;
  let is_label x = Option.exists ~f:P.is_label (I.component_opt x)
  let is_unused_label x ~symbol_table =
    Option.exists (I.component_opt x)
      ~f:(P.is_unused_label ~symbol_table)
  ;;
  let is_label_where x ~f = Option.exists ~f:(P.is_label_where ~f) (I.component_opt x)
  let is_jump_pair x y =
    Option.exists (Option.both (I.component_opt x) (I.component_opt y))
      ~f:(Tuple2.uncurry P.is_jump_pair)
  ;;
end

module type S_properties = sig
  type t
  include S_predicates with type t := t
  val exists
    :  ?directive:(string -> bool)
    -> ?instruction:(Abstract_instruction.with_operands -> bool)
    -> ?label:(Abstract_symbol.t -> bool)
    -> ?blank:bool
    -> ?other:bool
    -> t -> bool
  ;;
  val iter
    :  ?directive:(string -> unit)
    -> ?instruction:(Abstract_instruction.with_operands -> unit)
    -> ?label:(Abstract_symbol.t -> unit)
    -> ?blank:(unit -> unit)
    -> ?other:(unit -> unit)
    -> t -> unit
  ;;
  val flags : t -> Abstract_symbol.Table.t -> Flag.Set.t
end

module Inherit_properties
    (P : S_properties) (I : Utils.Inherit.S with type c := P.t)
  : S_properties with type t := I.t = struct

  module I_with_c = struct
    type c = P.t
    include I
  end
  include Inherit_predicates (P) (Utils.Inherit.Make_partial (I_with_c))

  let iter ?directive ?instruction ?label ?blank ?other x =
    P.iter ?directive ?instruction ?label ?blank ?other (I.component x)
  ;;
  let exists ?directive ?instruction ?label ?blank ?other x =
    P.exists ?directive ?instruction ?label ?blank ?other (I.component x)
  ;;

  let flags x = P.flags (I.component x)
end

module Properties : S_properties with type t := t = struct
  let map
      ~directive
      ~instruction
      ~label
      ~blank
      ~other
      stm =
    Variants.map stm
      ~directive:(Fn.const directive)
      ~instruction:(Fn.const instruction)
      ~label:(Fn.const label)
      ~blank:(fun _ -> blank ())
      ~other:(fun _ -> other ())
  ;;

  let exists
      ?(directive = Fn.const false)
      ?(instruction = Fn.const false)
      ?(label = Fn.const false)
      ?(blank = false)
      ?(other = false)
      stm =
    map stm ~directive ~instruction ~label
      ~blank:(Fn.const blank) ~other:(Fn.const other)
  ;;

  let iter
      ?(directive = Fn.const ())
      ?(instruction = Fn.const ())
      ?(label = Fn.const ())
      ?(blank = Fn.const ())
      ?(other = Fn.const ())
      stm =
    map stm ~directive ~instruction ~label ~blank ~other
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

  include Abstract_instruction.Inherit_predicates
      (struct
        type t = Abstract_instruction.with_operands
        include (Abstract_instruction :
                   Abstract_instruction.S_properties with type t := t)
      end)
      (struct
        type nonrec t = t
        let component_opt = function
          | Instruction i -> Some i
          | _ -> None
        ;;
      end)

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
