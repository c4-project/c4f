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

include Explanation_intf

let pp_details (pp_header : 'h Fmt.t) (pp_body : 'b Fmt.t) : ('h * 'b) Fmt.t =
  Fmt.(
    hvbox ~indent:4 (append (suffix sp (hvbox pp_header)) (braces pp_body)))

let pp_named_details (name : string) (pp_body : 'b Fmt.t) : 'b Fmt.t =
  Fmt.(using (fun x -> ((), x)) (pp_details (const string name) pp_body))

let pp_optional_details (name : string) (pp_item : 'b Fmt.t) : 'b option Fmt.t =
  Fmt.option (pp_named_details name pp_item)

let pp_listed_details (name : string) (pp_item : 'b Fmt.t) (f : Formatter.t)
  : 'b list -> unit = function
  | [] ->
    ()
  | ts ->
    Fmt.(pp_named_details name (list ~sep:comma pp_item)) f ts

module Make (B : Basic) :
  S
  with type elt := B.elt
   and type context := B.context
   and type details := B.details
   and module Abs := B.Abs
   and module Flag := B.Flag = struct
  type t =
    { original: B.elt
    ; abstract: B.Abs.t
    ; abs_kind: B.Abs.Kind.t
    ; abs_flags: B.Flag.Set.t
    ; details: B.details }
  [@@deriving fields]

  let has_abs_kind k x = B.Abs.Kind.equal k x.abs_kind

  let abs_kind_in ks x = B.Abs.Kind.Set.mem ks x.abs_kind

  let make ~context ~original =
    { original
    ; details= B.make_details original context
    ; abstract= B.abstract original
    ; abs_kind= B.abs_kind original
    ; abs_flags= B.abs_flags original context }

  let pp_body f t =
    Fmt.(
      Fields.Direct.iter t
        ~original:(fun _ _ _ -> ())
        ~abs_kind:(fun _ _ -> pf f "@[kind:@ %a;@]@ " B.Abs.Kind.pp)
        ~abstract:(fun _ _ _ -> ())
        ~abs_flags:(fun _ _ flags ->
            if not (B.Flag.Set.is_empty flags) then
              pf f "@[flags:@ %a;@]@ " B.Flag.pp_set flags )
        ~details:(fun _ _ -> B.pp_details f))

  let pp f t = pp_details B.pp pp_body f (original t, t)
end

module Make_loc (L : Act_language.Location.S) :
  S
  with type elt := L.t
   and type context := Act_abstract.Symbol.Table.t
   and module Abs := Act_abstract.Location = struct
  module Flag = Act_abstract.Location.Flag

  module Base = struct
    module Abs = Act_abstract.Location
    module Flag = Flag

    type elt = L.t

    let pp = L.pp

    type context = Act_abstract.Symbol.Table.t

    type details = unit

    let make_details _ _ = ()

    let pp_details _f _context = ()

    include Act_abstract.Abstractable.Make (struct
        module Abs = Abs

        type t = elt

        let abstract = L.abstract
      end)

    let abs_flags _ _ = Flag.Set.empty
  end

  type details = Base.details

  include Make (Base)
end

module Make_ops (L : Act_language.Instruction.S) :
  S
  with type elt := L.t
   and type context := Act_abstract.Symbol.Table.t
   and module Abs := Act_abstract.Operand.Bundle = struct
  module Flag = Act_abstract.Operand.Bundle.Flag

  module Base = struct
    module Abs = Act_abstract.Operand.Bundle
    module Flag = Flag

    type elt = L.t

    let pp = L.pp_operands

    type context = Act_abstract.Symbol.Table.t

    type details = unit

    let make_details _ _ = ()

    let pp_details _f _context = ()

    include Act_abstract.Abstractable.Make (struct
        module Abs = Abs

        type t = elt

        let abstract = L.abs_operands
      end)

    let abs_flags ins =
      Act_abstract.Operand.Bundle.flags
        (L.abs_operands ins)
  end

  type details = Base.details

  include Make (Base)
end

module Make_ins (B : Basic_ins) :
  S
  with type elt := B.Instruction.t
   and type context := Act_abstract.Symbol.Table.t
   and module Abs := Act_abstract.Instruction
 = struct
    module Flag = Act_abstract.Instruction.Flag

    module Base = struct
      module Abs = Act_abstract.Instruction
      module Flag = Flag

      type elt = B.Instruction.t

      let pp = B.Instruction.pp

      type context = Act_abstract.Symbol.Table.t

      type details =
        { operands: B.Ops_expl.t option
        ; locations: B.Loc_expl.t list }
      [@@deriving fields]

      let make_operand_details ins context =
        if B.Instruction.On_operands.is_none ins then None
        else Some (B.Ops_expl.make ~context ~original:ins)

      let make_location_details ins context =
        let locations = B.Instruction.On_locations.to_list ins in
        List.map locations ~f:(fun original ->
            B.Loc_expl.make ~context ~original )

      let make_details ins context =
        { operands= make_operand_details ins context
        ; locations= make_location_details ins context }

      let pp_operand_details =
        pp_optional_details "operands" B.Ops_expl.pp

      let pp_location_details =
        pp_listed_details "location" B.Loc_expl.pp

      let pp_details f {operands; locations} =
        Fmt.pf f "%a@ %a" pp_operand_details operands pp_location_details
          locations

      include (
        B.Instruction :
          Act_abstract.Abstractable.S
          with module Abs := Abs
           and type t := elt )

      let abs_flags _ _ = Flag.Set.empty
    end

  type details = Base.details

  include Make (Base)
end

module Make_stm (B : Basic_stm) = struct
    module Flag = B.Statement.Extended_flag

    type details = {instructions: B.Ins_expl.t list}
    [@@deriving fields]

    module Base = struct
      module Abs = Act_abstract.Statement
      module Flag = Flag

      type elt = B.Statement.t

      let pp = B.Statement.pp

      type context = Act_abstract.Symbol.Table.t

      type nonrec details = details

      let describe_instructions stm context =
        match B.Statement.abs_kind stm with
        | Act_abstract.Statement.Kind.Instruction ->
            stm |> B.Statement.On_instructions.to_list
            |> List.map ~f:(fun original ->
                   B.Ins_expl.make ~original ~context )
        | Blank | Unknown | Directive | Label ->
            []

      let make_details stm context =
        {instructions= describe_instructions stm context}

      let pp_instruction_details =
        pp_listed_details "instruction" B.Ins_expl.pp

      let pp_details f =
        Fields_of_details.Direct.iter ~instructions:(fun _ _ ->
            pp_instruction_details f )

      include (
        B.Statement :
          Act_abstract.Abstractable.S
          with module Abs := Abs
           and type t := elt )

      let abs_flags = B.Statement.extended_flags
    end

    include Make (Base)
end
