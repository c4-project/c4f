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
    with type Symbol.t      = B.Symbol.t
     and type Constant.t    = B.Constant.t
     and type Location.t    = B.Location.t
     and type Instruction.t = B.Instruction.t
     and type Statement.t   = B.Statement.t = struct
  include (B : Basic_core)

  module Symbol = Language_symbol.Make (B.Symbol)
  module Constant = Language_constant.Make (B.Constant)
  module Location = Language_location.Make (struct
      module Symbol = Symbol
      include B.Location
    end)
  module Instruction = Language_instruction.Make (struct
      module Constant = Constant
      module Symbol   = Symbol
      module Location = Location
      include B.Instruction
    end)
  module Statement = Language_statement.Make (struct
      module Symbol = Symbol
      module Instruction = Instruction
      include B.Statement
    end)


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
