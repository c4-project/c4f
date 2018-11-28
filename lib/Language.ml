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
open Utils

include Language_intf

module Make (B : Basic)
  : S
    with type Constant.t    = B.Constant.t
     and type Location.t    = B.Location.t
     and type Instruction.t = B.Instruction.t
     and type Statement.t   = B.Statement.t
     and type Symbol.t      = B.Symbol.t = struct
  include (B : Basic_core)

  module Symbol = Language_symbol.Make (B.Symbol)

  module Instruction = struct
    include B.Instruction

    include Abstract.Instruction.Inherit_properties
        (Abstract.Instruction)
        (struct
          type nonrec t = t
          let component = abstract
        end)
    ;;

    module On_operands =
      Abstract.Operand.Bundle.Inherit_properties
        (Abstract.Operand.Bundle)
        (struct
          type nonrec t = t
          let component = abs_operands
        end)
    ;;
  end

  module Statement = struct
    include B.Statement

    module Extended_flag = struct
      module M = struct
        type t =
          [ Abstract.Statement.Flag.t
          | `Program_boundary
          ] [@@deriving sexp, eq, enumerate]

        let table =
          (Abstract.Statement.Flag.table :> (t, string) List.Assoc.t)
          @ [ `Program_boundary, "program boundary" ]
      end

      include M
      include Enum.Extend_table (struct
          include M
          include Enum.Make_from_enumerate (M)
        end)
    end

    include Abstract.Statement.Inherit_properties
        (Abstract.Statement)
        (struct
          type nonrec t = t
          let component = abstract
        end)
    ;;

    let is_program_boundary stm =
      is_label stm
      && On_symbols.for_all stm ~f:Symbol.is_program_label
    ;;

    let is_unused_ordinary_label stm ~symbol_table =
      not (is_program_boundary stm)
      && is_unused_label stm ~symbol_table
    ;;

    let coerced_flags stm symbol_table =
      let abs_flags = flags stm symbol_table in
      let coerce x = (x :> Extended_flag.t) in
      Extended_flag.Set.map abs_flags ~f:coerce
    ;;

    let new_flags stm _symbol_table =
      if is_program_boundary stm
      then Extended_flag.Set.singleton `Program_boundary
      else Extended_flag.Set.empty
    ;;

    let extended_flags stm symbol_table =
      Extended_flag.Set.union
        (coerced_flags stm symbol_table)
        (new_flags stm symbol_table)
    ;;
  end

  module Location = struct
    include B.Location

    include Abstract.Location.Inherit_predicates
        (Abstract.Location)
        (struct
          type nonrec t = t
          let component_opt x = Some (abstract x)
        end)
    ;;
  end

  module Constant = struct
    include B.Constant
  end

  let heap_symbols_of_instruction ins ~known_heap_symbols =
    let symbols_in_heap_position =
      ins
      |> Instruction.On_locations.to_list
      |> List.filter_map ~f:Location.as_heap_symbol
    in
    let compare_against_known sym =
      let asym = Symbol.abstract sym in
      let is_known = Abstract.Symbol.Set.mem known_heap_symbols asym in
      Option.some_if is_known asym
    in
    let symbols_matching_known =
      ins
      |> Instruction.On_symbols.to_list
      |> List.filter_map ~f:compare_against_known
    in
    symbols_in_heap_position @ symbols_matching_known
  ;;

  let heap_symbols prog ~known_heap_symbols =
    prog
    |> List.concat_map ~f:Statement.On_instructions.to_list
    (* In x86, at least, jumps can contain locations that look without
       context to be heap symbols.  Currently, we pessimistically
       exclude any location that's inside a jump.

       Maybe, one day, we'll find an architecture that does actually
       contain heap locations in a jump, and have to re-think this. *)
    |> My_list.exclude ~f:Instruction.is_jump
    |> List.concat_map
      ~f:(heap_symbols_of_instruction ~known_heap_symbols)
    |> Abstract.Symbol.Set.of_list

  (** [symbols_in_statements_where filter prog] collects all
      abstract symbols belonging to statements in [prog] that match
      the filtering predicate [filter]. *)
  let symbols_in_statements_where filter prog =
    prog
    |> List.filter_map
      ~f:(fun p ->
          if filter p
          then Some (Symbol.Set.of_list (Statement.On_symbols.to_list p))
          else None)
    |> Symbol.Set.union_list
    |> Symbol.Set.abstract
  ;;

  let jump_symbols = symbols_in_statements_where Statement.is_jump
  let label_symbols = symbols_in_statements_where Statement.is_label

  let symbols prog ~known_heap_symbols =
    Abstract.(
      Symbol.Table.of_sets
        [ heap_symbols  prog ~known_heap_symbols, Symbol.Sort.Heap
        ; jump_symbols  prog                    , Symbol.Sort.Jump
        ; label_symbols prog                    , Symbol.Sort.Label
        ]
    )
  ;;
end
