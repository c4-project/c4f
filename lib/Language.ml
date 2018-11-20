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

    include Abstract_instruction.Inherit_properties
        (Abstract_instruction)
        (struct
          type nonrec t = t
          let component = abs_type
        end)
    ;;
  end

  module Statement = struct
    include B.Statement

    module Flag = struct
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

    include Abstract_statement.Inherit_properties
        (Abstract_statement)
        (struct
          type nonrec t = t
          let component = abs_type
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

    let flags stm symbol_table =
      let abs_flags = flags stm symbol_table in
      Flag.Set.union
        (Flag.Set.map abs_flags ~f:(fun x -> (x :> Flag.t)))
        (if is_program_boundary stm
         then Flag.Set.singleton `Program_boundary
         else Flag.Set.empty)
    ;;
  end

  module Location = struct
    include B.Location

    let to_heap_symbol l =
      match abs_type l with
      | Abstract.Location.Heap s -> Some s
      | _ -> None
  end

  module Constant = struct
    include B.Constant
  end

  let heap_symbols prog =
    prog
    |> List.concat_map ~f:Statement.On_instructions.to_list
    (* In x86, at least, jumps can contain locations that look without
       context to be heap symbols.  Currently, we pessimistically
       exclude any location that's inside a jump.

       Maybe, one day, we'll find an architecture that does actually
       contain heap locations in a jump, and have to re-think this. *)
    |> My_list.exclude ~f:Instruction.is_jump
    |> List.concat_map ~f:Instruction.On_locations.to_list
    |> List.filter_map ~f:Location.to_heap_symbol
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

  let symbols prog =
    Abstract.(
      Symbol.Table.of_sets
        [ heap_symbols  prog, Symbol.Sort.Heap
        ; jump_symbols  prog, Symbol.Sort.Jump
        ; label_symbols prog, Symbol.Sort.Label
        ]
    )
  ;;
end
