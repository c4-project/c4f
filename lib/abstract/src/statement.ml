(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
module Au = Act_utils

type t =
  | Directive of string
  | Instruction of Instruction.t
  | Blank
  | Label of Symbol.t
  | Unknown
[@@deriving sexp, variants]

let pp f = function
  | Blank ->
      ()
  | Directive d ->
      Fmt.pf f ".%s" d
  | Label l ->
      Fmt.pf f ":%s" l
  | Instruction ins ->
      Instruction.pp f ins
  | Unknown ->
      String.pp f "??"

module Kind = struct
  module M = struct
    type t = Directive | Instruction | Blank | Label | Unknown
    [@@deriving enum, sexp]

    let table =
      [ (Directive, "directive")
      ; (Instruction, "instruction")
      ; (Blank, "blank")
      ; (Label, "label")
      ; (Unknown, "unknown") ]
  end

  include M
  include Au.Enum.Extend_table (M)
end

let kind = function
  | Directive _ ->
      Kind.Directive
  | Instruction _ ->
      Instruction
  | Blank ->
      Blank
  | Label _ ->
      Label
  | Unknown ->
      Unknown

module Flag = struct
  module M = struct
    type t = [`UnusedLabel | `StackManip] [@@deriving enum, enumerate, sexp]

    let table =
      [(`UnusedLabel, "unused label"); (`StackManip, "manipulates stack")]
  end

  include M
  include Au.Enum.Extend_table (M)
end

module type S_predicates = sig
  type t

  include Instruction.S_predicates with type t := t

  val is_directive : t -> bool

  val is_instruction : t -> bool

  val is_instruction_where : t -> f:(Instruction.t -> bool) -> bool

  val is_label : t -> bool

  val is_label_where : t -> f:(string -> bool) -> bool

  val is_unused_label : t -> symbol_table:Symbol.Table.t -> bool

  val is_jump_pair : t -> t -> bool

  val is_blank : t -> bool

  val is_unknown : t -> bool
end

module Inherit_predicates
    (P : S_predicates)
    (I : Au.Inherit_types.S_partial with type c := P.t) :
  S_predicates with type t := I.t = struct
  open Option

  include Instruction.Inherit_predicates
            (P)
            (struct
              type t = I.t

              let component_opt = I.component_opt
            end)

  let is_directive x = exists ~f:P.is_directive (I.component_opt x)

  let is_blank x = exists ~f:P.is_blank (I.component_opt x)

  let is_unknown x = exists ~f:P.is_unknown (I.component_opt x)

  let is_instruction x = exists ~f:P.is_instruction (I.component_opt x)

  let is_instruction_where x ~f =
    exists ~f:(P.is_instruction_where ~f) (I.component_opt x)

  let is_label x = exists ~f:P.is_label (I.component_opt x)

  let is_unused_label x ~symbol_table =
    exists (I.component_opt x) ~f:(P.is_unused_label ~symbol_table)

  let is_label_where x ~f =
    exists (I.component_opt x) ~f:(P.is_label_where ~f)

  let is_jump_pair x y =
    exists
      (both (I.component_opt x) (I.component_opt y))
      ~f:(fun (x, y) -> P.is_jump_pair x y)
end

module type S_properties = sig
  type t

  include S_predicates with type t := t

  val exists :
       ?directive:(string -> bool)
    -> ?instruction:(Instruction.t -> bool)
    -> ?label:(Symbol.t -> bool)
    -> ?blank:bool
    -> ?unknown:bool
    -> t
    -> bool

  val iter :
       ?directive:(string -> unit)
    -> ?instruction:(Instruction.t -> unit)
    -> ?label:(Symbol.t -> unit)
    -> ?blank:(unit -> unit)
    -> ?unknown:(unit -> unit)
    -> t
    -> unit

  val flags : t -> Symbol.Table.t -> Set.M(Flag).t
end

module Inherit_properties
    (P : S_properties)
    (I : Au.Inherit_types.S with type c := P.t) :
  S_properties with type t := I.t = struct
  module I_with_c = struct
    type c = P.t

    include I
  end

  include Inherit_predicates (P) (Au.Inherit.Make_partial (I_with_c))

  let iter ?directive ?instruction ?label ?blank ?unknown x =
    P.iter ?directive ?instruction ?label ?blank ?unknown (I.component x)

  let exists ?directive ?instruction ?label ?blank ?unknown x =
    P.exists ?directive ?instruction ?label ?blank ?unknown (I.component x)

  let flags x = P.flags (I.component x)
end

module Properties : S_properties with type t := t = struct
  let map ~directive ~instruction ~label ~blank ~unknown stm =
    Variants.map stm ~directive:(Fn.const directive)
      ~instruction:(Fn.const instruction) ~label:(Fn.const label)
      ~blank:(fun _ -> blank ())
      ~unknown:(fun _ -> unknown ())

  let exists ?(directive = Fn.const false) ?(instruction = Fn.const false)
      ?(label = Fn.const false) ?(blank = false) ?(unknown = false) stm =
    map stm ~directive ~instruction ~label ~blank:(Fn.const blank)
      ~unknown:(Fn.const unknown)

  let iter ?(directive = Fn.const ()) ?(instruction = Fn.const ())
      ?(label = Fn.const ()) ?(blank = Fn.const ()) ?(unknown = Fn.const ())
      stm =
    map stm ~directive ~instruction ~label ~blank ~unknown

  let is_directive stm = exists ~directive:(Fn.const true) stm

  let is_blank stm = exists ~blank:true stm

  let is_unknown stm = exists ~unknown:true stm

  let is_label_where stm ~f = exists ~label:f stm

  let is_label = is_label_where ~f:(Fn.const true)

  let is_unused_label stm ~symbol_table =
    is_label_where stm ~f:(fun label ->
        Symbol.(not (Table.mem symbol_table label ~sort:Sort.Jump)))

  let is_instruction_where stm ~f = exists ~instruction:f stm

  let is_instruction = is_instruction_where ~f:(Fn.const true)

  include Instruction.Inherit_predicates
            (Instruction)
            (struct
              type nonrec t = t

              let component_opt = function
                | Instruction i ->
                    Some i
                | _ ->
                    None
            end)

  let is_jump_pair j l =
    is_symbolic_jump_where j ~f:(fun j_sym ->
        is_label_where l ~f:(Symbol.equal j_sym))

  let flags stm symbol_table =
    [ (is_unused_label ~symbol_table stm, `UnusedLabel)
    ; (is_stack_manipulation stm, `StackManip) ]
    |> List.filter_map ~f:(fun (x, y) -> Option.some_if x y)
    |> Set.of_list (module Flag)
end

include Properties
