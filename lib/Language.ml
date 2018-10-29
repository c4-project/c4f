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
open Utils.MyContainers

include Language_intf

module Make (M : S)
  : Intf
    with type Constant.t    = M.Constant.t
     and type Location.t    = M.Location.t
     and type Instruction.t = M.Instruction.t
     and type Statement.t   = M.Statement.t
     and type Symbol.t      = M.Symbol.t = struct
  include (M : BaseS)

  module Symbol = Language_symbol.Make (M.Symbol)

  module Instruction = struct
    include M.Instruction

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (Symbol.Set)
    module OnLocations = FoldMap.Make (OnLocationsS)

    let is_jump ins =
      match abs_type ins with
      | Abstract.Instruction.Jump -> true
      | _ -> false

    let is_stack_manipulation ins =
      Abstract.(
        match abs_type ins with
        | Instruction.Stack -> true
        | Instruction.Arith -> begin
            (* Stack pointer movements *)
            match abs_operands ins with
            | Operands.IntImmediate
                { src = _
                ; dst = Location.StackPointer
                }
              -> true
            | _ -> false
          end
        | Instruction.Move -> begin
            (* Stack pointer transfers *)
            match abs_operands ins with
            | Operands.LocTransfer
                { src = Location.StackPointer
                ; dst = _
                }
            | Operands.LocTransfer
                { src = _
                ; dst = Location.StackPointer
                }
              -> true
            | _ -> false
          end
        | _ -> false
      )
    ;;

    let mem s i = Abstract.Instruction.Set.mem s (abs_type i)
  end

  module Statement = struct
    include M.Statement

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (Symbol.Set)
    module OnInstructions = FoldMap.Make (OnInstructionsS)

    let is_jump =
      OnInstructions.exists ~f:Instruction.is_jump

    let is_stack_manipulation =
      OnInstructions.exists ~f:Instruction.is_stack_manipulation

    let instruction_mem s =
      OnInstructions.exists ~f:(Instruction.mem s)

    let is_directive stm =
      match abs_type stm with
      | Abstract.Statement.Directive _ -> true
      | _ -> false

    let is_label stm =
      match abs_type stm with
      | Abstract.Statement.Label _ -> true
      | _ -> false

    (** [is_label_and p x] returns [p x] if [x] is a label, or
        [false] otherwise. *)
    let is_label_and p = MyFn.conj is_label p;;

    let is_nop stm =
      Abstract.(
        match abs_type stm with
        | Statement.Blank -> true
        | Statement.Instruction Instruction.Nop -> true
        | _ -> false
      )

    let is_program_boundary =
      is_label_and (OnSymbols.exists ~f:Symbol.is_program_label)
    ;;

    let is_unused_label ?(ignore_boundaries=false) ~syms =
      is_label_and
        (fun stm ->
           let jsyms = Abstract.Symbol.(Table.set_of_sort syms Sort.Jump) in
           let ssyms = Symbol.Set.abstract (OnSymbols.set stm) in
           Abstract.Symbol.Set.disjoint jsyms ssyms
           && not (ignore_boundaries && is_program_boundary stm)
        )
    ;;

    let is_jump_pair x y =
      is_jump x
      && is_label y
      && (MyFn.on OnSymbols.set OnSymbols.Set.equal) x y
    ;;

    let flags ~syms stm =
      [ is_unused_label ~syms  stm, `UnusedLabel
      ; is_program_boundary    stm, `ProgBoundary
      ; is_stack_manipulation  stm, `StackManip
      ]
      |> List.map ~f:(Tuple2.uncurry Option.some_if)
      |> List.filter_opt
      |> Abstract.Statement.Flag.Set.of_list
    ;;
  end

  module Location = struct
    include M.Location

    let to_heap_symbol l =
      match abs_type l with
      | Abstract.Location.Heap s -> Some s
      | _ -> None
  end

  module Constant = struct
    include M.Constant
  end

  let heap_symbols prog =
    prog
    |> List.concat_map ~f:Statement.OnInstructions.list
    (* In x86, at least, jumps can contain locations that look without
       context to be heap symbols.  Currently, we pessimistically
       exclude any location that's inside a jump.

       Maybe, one day, we'll find an architecture that does actually
       contain heap locations in a jump, and have to re-think this. *)
    |> MyList.exclude ~f:Instruction.is_jump
    |> List.concat_map ~f:Instruction.OnLocations.list
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
          then Some (Statement.OnSymbols.set p)
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
