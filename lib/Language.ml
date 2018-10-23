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

module type BaseS = sig
  val name : string

  val is_program_label : string -> bool

  val pp_comment
    :  pp:(Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a
    -> unit
end

module type StatementS = sig
  type t
  type ins

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t
  module OnInstructionsS
    : FoldMap.S with type t = ins
                 and type cont = t

  val empty : unit -> t
  val label : string -> t
  val instruction : ins -> t
  val abs_type : t -> Abstract.Statement.t
end

module type InstructionS = sig
  type t
  type loc

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t
  module OnLocationsS
    : FoldMap.S with type t = loc
                 and type cont = t

  val jump : string -> t

  val abs_operands : t -> Abstract.Operands.t
  val abs_type : t -> Abstract.Instruction.t
end

module type LocationS = sig
  type t

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  val make_heap_loc : string -> t
  val abs_type : t -> Abstract.Location.t
end

module type ConstantS = sig
  type t

  include Core.Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  val zero : t
end

module type S = sig
  include BaseS

  module Constant : ConstantS
  module Location : LocationS
  module Instruction : InstructionS with type loc = Location.t
  module Statement : StatementS with type ins = Instruction.t
end

module type Intf = sig
  include BaseS

  module Constant : sig
    include ConstantS
  end

  module Location : sig
    include LocationS

    val to_heap_symbol : t -> string option
  end

  module Instruction : sig
    include InstructionS

    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t
    module OnLocations
      : FoldMap.Intf with type t = Location.t
                      and type cont = t

    val mem : Abstract.Instruction.Set.t -> t -> bool
    val is_jump : t -> bool
    val is_stack_manipulation : t -> bool
  end

  module Statement : sig
    include StatementS with type ins = Instruction.t

    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t
    module OnInstructions
      : FoldMap.Intf with type t = Instruction.t
                      and type cont = t


    val instruction_mem : Abstract.Instruction.Set.t -> t -> bool
    val is_jump : t -> bool
    val is_stack_manipulation : t -> bool

    val is_directive : t -> bool

    val is_label : t -> bool
    val is_unused_label
      :  ?ignore_boundaries:bool
      -> syms:Abstract.Symbol.Table.t
      -> t
      -> bool
    val is_jump_pair : t -> t -> bool

    val is_nop : t -> bool
    val is_program_boundary : t -> bool

    val flags
      :  syms:Abstract.Symbol.Table.t
      -> t
      -> Abstract.Statement.Flag.Set.t
  end

  val symbols : Statement.t list -> Abstract.Symbol.Table.t
end

module Make (M : S)
  : Intf with type Constant.t    = M.Constant.t
          and type Location.t    = M.Location.t
          and type Instruction.t = M.Instruction.t
          and type Statement.t   = M.Statement.t = struct
  include (M : BaseS)

  module Instruction = struct
    include M.Instruction

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (Abstract.Symbol.Set)
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

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (Abstract.Symbol.Set)
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

    let is_nop stm =
      Abstract.(
        match abs_type stm with
        | Statement.Blank -> true
        | Statement.Instruction Instruction.Nop -> true
        | _ -> false
      )

    let is_program_boundary stm =
      Abstract.(
        match abs_type stm with
        | Statement.Label l -> is_program_label l
        | _ -> false
      )

    let is_unused_label ?(ignore_boundaries=false) ~syms stm =
      let jsyms = Abstract.Symbol.(Table.set_of_sort syms Sort.Jump) in
      is_label stm
      && Abstract.Symbol.Set.disjoint jsyms (OnSymbols.set stm)
      && not (ignore_boundaries && is_program_boundary stm)

    let is_jump_pair x y =
      is_jump x
      && is_label y
      && (MyFn.on OnSymbols.set OnSymbols.Set.equal) x y

    let flags ~syms stm =
      [ is_unused_label ~syms  stm, `UnusedLabel
      ; is_program_boundary    stm, `ProgBoundary
      ; is_stack_manipulation  stm, `StackManip
      ]
      |> List.map ~f:(Tuple2.uncurry Option.some_if)
      |> List.filter_opt
      |> Abstract.Statement.Flag.Set.of_list
  end

  module Location =
  struct
    include M.Location

    let to_heap_symbol l =
      match abs_type l with
      | Abstract.Location.Heap s -> Some s
      | _ -> None
  end

  module Constant =
  struct
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

  let jump_symbols prog =
    prog
    |> List.filter ~f:Statement.is_jump
    |> List.map ~f:Statement.OnSymbols.set
    |> Abstract.Symbol.Set.union_list

  let label_symbols prog =
    prog
    |> List.filter ~f:Statement.is_label
    |> List.map ~f:Statement.OnSymbols.set
    |> Abstract.Symbol.Set.union_list

  let symbols prog =
    Abstract.(
      Symbol.Table.of_sets
        [ heap_symbols prog , Symbol.Sort.Heap
        ; jump_symbols prog , Symbol.Sort.Jump
        ; label_symbols prog, Symbol.Sort.Label
        ]
    )
  ;;
end
