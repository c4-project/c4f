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

type name =
  | X86 of X86Dialect.t
[@@deriving sexp]

let pp_name ?(show_sublang=true) f =
  function
  | X86 syn ->
    Format.pp_open_box f 0;
    Format.pp_print_string f "X86";
    if show_sublang
    then Format.fprintf f "@ (%a)" X86Dialect.pp syn;
    Format.pp_close_box f ()

let%expect_test "pp_name: explicitly showing sublang" =
  Format.printf "%a@."
    (pp_name ~show_sublang:true) (X86 X86Dialect.Att);
  [%expect {| X86 (AT&T) |}]

let%expect_test "pp_name: explicitly not showing sublang" =
  Format.printf "%a@."
    (pp_name ~show_sublang:false) (X86 X86Dialect.Intel);
  [%expect {| X86 |}]

let%expect_test "pp_name: default" =
  Format.printf "%a@."
    (fun a -> pp_name a) (X86 X86Dialect.Herd7);
  [%expect {| X86 (Herd7) |}]

module AbsInstruction = struct
  type t =
    | Arith
    | Fence
    | Jump
    | Move
    | Nop
    | Call
    | Stack
    | Other [@@deriving enum, sexp]

  module Table =
    StringTable.Make (
    struct
      type nonrec t = t

      let table =
        [ Arith, "arith"
        ; Fence, "fence"
        ; Jump , "jump"
        ; Move , "move"
        ; Nop  , "nop"
        ; Call , "call"
        ; Stack, "stack"
        ; Other, "other"
        ]
    end
    )

  let pp f ins =
    ins
    |> Table.to_string
    |> Option.value ~default:"??"
    |> String.pp f

  module Set =
    Set.Make(
    struct
      type nonrec t = t

      let compare x y =
        Int.compare (to_enum x)
          (to_enum y)

      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end
    )

end

module AbsLocation =
struct
  type t =
    | StackPointer
    | StackOffset of int
    | Heap of string
    | GeneralRegister
    | Unknown

  let pp f =
    function
    | StackPointer      -> String.pp      f "&stack"
    | StackOffset     i -> Format.fprintf f "stack[%d]" i
    | Heap            s -> Format.fprintf f "heap[%s]" s
    | GeneralRegister   -> String.pp      f "reg"
    | Unknown           -> String.pp      f "??"
end

module AbsStatement = struct
  type t =
    | Directive of string
    | Instruction of AbsInstruction.t
    | Blank
    | Label of string
    | Other

  let pp f =
    function
    | Blank         -> ()
    | Directive   d -> Format.fprintf f "directive@ (%s)" d
    | Label       l -> Format.fprintf f ":%s"             l
    | Instruction i -> AbsInstruction.pp f i
    | Other         -> String.pp f "??"

  type flag =
    [ `UnusedLabel
    | `ProgBoundary
    | `StackManip
    ] [@@deriving enum, sexp]

  module FlagTable =
    StringTable.Make (
    struct
      type t = flag

      let table =
        [ `UnusedLabel, "unused label"
        ; `ProgBoundary, "program boundary"
        ; `StackManip, "manipulates stack"
        ]
    end
    )

  let pp_flag f flag =
    flag
    |> FlagTable.to_string
    |> Option.value ~default:"??"
    |> String.pp f

  module FlagSet = struct
    include
      Set.Make(
      struct
        type t = flag

        let compare x y =
          Int.compare (flag_to_enum x)
            (flag_to_enum y)

        let sexp_of_t = sexp_of_flag
        let t_of_sexp = flag_of_sexp
      end
      )

    let pp f fset =
      match Set.to_list fset with
      | [] -> ()
      | xs ->
        Format.pp_print_space f ();
        Format.pp_print_char f '(';
        Format.pp_open_hovbox f 0;
        Format.pp_print_list ~pp_sep:MyFormat.pp_csep pp_flag f xs;
        Format.pp_close_box f ();
        Format.pp_print_char f ')'
  end
end

module AbsOperands = struct
  type t =
    | None
    | LocTransfer of (AbsLocation.t, AbsLocation.t) SrcDst.t
    | IntImmediate of (int, AbsLocation.t) SrcDst.t
    | Erroneous
    | Other

  let pp f =
    function
    | None -> String.pp f "none"
    | LocTransfer {src; dst} ->
      Format.fprintf f "@[%a@ ->@ %a@]"
        AbsLocation.pp src
        AbsLocation.pp dst
    | IntImmediate {src; dst} ->
      Format.fprintf f "@[$%d@ ->@ %a@]"
        src
        AbsLocation.pp dst
    | Erroneous -> String.pp f "<invalid operands>"
    | Other -> String.pp f "??"
end

module SymSet = Set.Make(String)

module type BaseS = sig
  val name : name
  val is_program_label : string -> bool
end

module type StatementS = sig
  type t
  type ins

  include Pretty_printer.S with type t := t

  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t
  module OnInstructionsS
    : FoldMap.S with type t = ins
                 and type cont = t

  val empty : unit -> t
  val abs_type : t -> AbsStatement.t
end

module type InstructionS = sig
  type t
  type loc

  include Pretty_printer.S with type t := t

  module OnSymbolsS
    : FoldMap.S with type t = string
                 and type cont = t
  module OnLocationsS
    : FoldMap.S with type t = loc
                 and type cont = t

  val abs_operands : t -> AbsOperands.t
  val abs_type : t -> AbsInstruction.t
end

module type LocationS = sig
  type t

  include Pretty_printer.S with type t := t

  val make_heap_loc : string -> t
  val abs_type : t -> AbsLocation.t
end

module type ConstantS = sig
  type t
  include Core.Pretty_printer.S with type t := t
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
  end

  module Instruction : sig
    include InstructionS

    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t
    module OnLocations
      : FoldMap.Intf with type t = Location.t
                      and type cont = t

    val mem : AbsInstruction.Set.t -> t -> bool
    val is_jump : t -> bool
    val is_stack_manipulation : t -> bool
  end

  module Statement :
  sig
    include StatementS

    module OnSymbols
      : FoldMap.SetIntf with type t = string
                         and type cont = t
    module OnInstructions
      : FoldMap.Intf with type t = Instruction.t
                      and type cont = t


    val instruction_mem : AbsInstruction.Set.t -> t -> bool
    val is_jump : t -> bool
    val is_stack_manipulation : t -> bool

    val is_directive : t -> bool
    val is_label : t -> bool
    val is_unused_label : jsyms:SymSet.t -> t -> bool
    val is_nop : t -> bool
    val is_program_boundary : t -> bool

    val flags : jsyms:SymSet.t -> t -> AbsStatement.FlagSet.t
  end


  val jump_symbols : Statement.t list -> SymSet.t
end

module Make (M : S) = struct
  let name = M.name
  let is_program_label = M.is_program_label

  module Instruction = struct
    include M.Instruction

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (SymSet)
    module OnLocations = FoldMap.Make (OnLocationsS)

    let is_jump ins =
      match abs_type ins with
      | AbsInstruction.Jump -> true
      | _ -> false

    let is_stack_manipulation ins =
      match abs_type ins with
      | AbsInstruction.Stack -> true
      | AbsInstruction.Arith -> begin
          (* Stack pointer movements *)
          match abs_operands ins with
          | AbsOperands.IntImmediate
              { src = _
              ; dst = AbsLocation.StackPointer
              }
            -> true
          | _ -> false
        end
      | AbsInstruction.Move -> begin
          (* Stack pointer transfers *)
          match abs_operands ins with
          | AbsOperands.LocTransfer
              { src = AbsLocation.StackPointer
              ; dst = _
              }
          | AbsOperands.LocTransfer
              { src = _
              ; dst = AbsLocation.StackPointer
              }
            -> true
          | _ -> false
        end
      | _ -> false

    let mem s i = AbsInstruction.Set.mem s (abs_type i)
  end

  module Statement = struct
    include M.Statement

    module OnSymbols = FoldMap.MakeSet (OnSymbolsS) (SymSet)
    module OnInstructions = FoldMap.Make (OnInstructionsS)

    let is_jump =
      OnInstructions.exists ~f:Instruction.is_jump

    let is_stack_manipulation =
      OnInstructions.exists ~f:Instruction.is_stack_manipulation

    let instruction_mem s =
      OnInstructions.exists ~f:(Instruction.mem s)

    let is_directive stm =
      match abs_type stm with
      | AbsStatement.Directive _ -> true
      | _ -> false

    let is_label stm =
      match abs_type stm with
      | AbsStatement.Label _ -> true
      | _ -> false

    let is_unused_label ~jsyms stm =
      is_label stm
      && SymSet.is_empty
        (SymSet.inter jsyms (OnSymbols.set stm))

    let is_nop stm =
      match abs_type stm with
      | AbsStatement.Blank -> true
      | AbsStatement.Instruction AbsInstruction.Nop -> true
      | _ -> false

    let is_program_boundary stm =
      match abs_type stm with
      | AbsStatement.Label l -> is_program_label l
      | _ -> false

    let flags ~jsyms stm =
      [ is_unused_label ~jsyms stm, `UnusedLabel
      ; is_program_boundary    stm, `ProgBoundary
      ; is_stack_manipulation  stm, `StackManip
      ]
      |> List.map ~f:(Tuple2.uncurry Option.some_if)
      |> List.filter_opt
      |> AbsStatement.FlagSet.of_list
  end

  module Location =
  struct
    include M.Location
  end

  module Constant =
  struct
    include M.Constant
  end

  let jump_symbols prog =
    prog
    |> List.filter ~f:Statement.is_jump
    |> List.map ~f:Statement.OnSymbols.set
    |> SymSet.union_list
end
