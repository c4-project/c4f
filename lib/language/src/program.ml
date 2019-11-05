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
module Tx = Travesty_base_exts

module Make (B : Program_types.Basic_with_modules) :
  Program_types.S with type t = B.t and module Statement = B.Statement =
struct
  include B
  module Tr = Travesty.Traversable

  module On_statements = struct
    include Tr.Chain0
              (struct
                type t = B.t

                include On_listings
              end)
              (Tr.Fix_elt (Tx.List) (Statement))

    include Travesty.Filter_mappable.Make0 (struct
      type t = B.t

      type elt = B.Statement.t

      let filter_map x ~f = On_listings.map x ~f:(List.filter_map ~f)
    end)
  end

  let listing : t -> Statement.t list = On_statements.to_list

  module On_symbols =
    Travesty.Traversable.Chain0 (On_statements) (Statement.On_symbols)
  module Instruction = B.Statement.Instruction
  module Symbol = Instruction.Symbol
  module Location = Instruction.Location

  let heap_symbols_of_instruction ins ~known_heap_symbols =
    let symbols_in_heap_position =
      ins |> Instruction.On_locations.to_list
      |> List.filter_map ~f:Location.as_heap_symbol
    in
    let compare_against_known sym =
      let asym = Symbol.abstract sym in
      let is_known = Set.mem known_heap_symbols asym in
      Option.some_if is_known asym
    in
    let symbols_matching_known =
      ins |> Instruction.On_symbols.to_list
      |> List.filter_map ~f:compare_against_known
    in
    symbols_in_heap_position @ symbols_matching_known

  let heap_symbols prog ~known_heap_symbols =
    prog |> On_statements.to_list
    |> List.concat_map ~f:Statement.On_instructions.to_list
    (* In x86, at least, jumps can contain locations that look without
       context to be heap symbols. Currently, we pessimistically exclude any
       location that's inside a jump.

       Maybe, one day, we'll find an architecture that does actually contain
       heap locations in a jump, and have to re-think this. *)
    |> Tx.List.exclude ~f:Instruction.is_jump
    |> List.concat_map ~f:(heap_symbols_of_instruction ~known_heap_symbols)
    |> Set.of_list (module Act_abstract.Symbol)

  (** [symbols_in_statements_where filter prog] collects all abstract symbols
      belonging to statements in [prog] that match the filtering predicate
      [filter]. *)
  let symbols_in_statements_where filter prog =
    prog |> On_statements.to_list
    |> List.filter_map ~f:(fun p ->
           if filter p then
             Some
               (Set.of_list (module Symbol) (Statement.On_symbols.to_list p))
           else None)
    |> Set.union_list (module Symbol)
    |> Set.map (module Act_abstract.Symbol) ~f:Symbol.abstract

  let jump_symbols = symbols_in_statements_where Statement.is_jump

  let label_symbols = symbols_in_statements_where Statement.is_label

  let symbols prog ~(known_heap_symbols : Set.M(Act_abstract.Symbol).t) :
      Act_abstract.Symbol.Table.t =
    Act_abstract.(
      Symbol.Table.of_sets
        [ (heap_symbols prog ~known_heap_symbols, Symbol.Sort.Heap)
        ; (jump_symbols prog, Symbol.Sort.Jump)
        ; (label_symbols prog, Symbol.Sort.Label) ])

  let split_stms_on_boundaries stms =
    (* Adding a nop to the start forces there to be some instructions before
       the first program, meaning we can simplify discarding such
       instructions. *)
    let splits =
      B.Statement.empty () :: stms
      |> List.group ~break:(Fn.const B.Statement.is_program_boundary)
    in
    Or_error.return (List.drop splits 1)

  let split_on_boundaries prog = B.split prog ~f:split_stms_on_boundaries
end
