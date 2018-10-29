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

module Instruction = struct
  module M = struct
    type t =
      | Arith
      | Call
      | Compare
      | Fence
      | Jump
      | Logical
      | Move
      | Nop
      | Return
      | Rmw
      | Stack
      | Other
      | Unknown
    [@@deriving enum, sexp]
    ;;

    let table =
      [ Arith  , "arith"
      ; Call   , "call"
      ; Compare, "compare"
      ; Fence  , "fence"
      ; Jump   , "jump"
      ; Logical, "logical"
      ; Move   , "move"
      ; Nop    , "nop"
      ; Return , "return"
      ; Rmw    , "RMW"
      ; Stack  , "stack"
      ; Other  , "other"
      ; Unknown, "??"
      ]
  end

  include M
  include Enum.ExtendTable (M)
end

module Location = struct
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

module Statement = struct
  type t =
    | Directive of string
    | Instruction of Instruction.t
    | Blank
    | Label of string
    | Other

  let pp f =
    function
    | Blank         -> ()
    | Directive   d -> Format.fprintf f "directive@ (%s)" d
    | Label       l -> Format.fprintf f ":%s"             l
    | Instruction i -> Instruction.pp f i
    | Other         -> String.pp f "??"

  module Flag = struct
    module M = struct
      type t =
        [ `UnusedLabel
        | `ProgBoundary
        | `StackManip
        ] [@@deriving enum, sexp]

      let table =
        [ `UnusedLabel, "unused label"
        ; `ProgBoundary, "program boundary"
        ; `StackManip, "manipulates stack"
        ]
    end

    include M
    include Enum.ExtendTable (M)
  end
end

module Operands = struct
  type t =
    | None
    | LocTransfer of (Location.t, Location.t) SrcDst.t
    | IntImmediate of (int, Location.t) SrcDst.t
    | SymbolicJump of string
    | Erroneous
    | Other
    | Unknown

  let pp f = function
    | None -> String.pp f "none"
    | LocTransfer {src; dst} ->
      Format.fprintf f "@[%a@ ->@ %a@]"
        Location.pp src
        Location.pp dst
    | IntImmediate {src; dst} ->
      Format.fprintf f "@[$%d@ ->@ %a@]"
        src
        Location.pp dst
    | SymbolicJump s ->
      Format.fprintf f "@[jump->%s@]" s
    | Erroneous -> String.pp f "<invalid operands>"
    | Other -> String.pp f "other"
    | Unknown -> String.pp f "??"
  ;;
end

module Symbol = struct
  type t = string

  module Set = struct
    module S = Set.Make (String)
    include S
    include MyContainers.SetExtend (S)
  end

  module Sort = struct
    module M = struct
      type t =
        | Jump
        | Heap
        | Label
          [@@deriving enum, sexp]

      let table =
        [ Jump,  "jump"
        ; Heap,  "heap"
        ; Label, "label"
        ]
    end

    include M
    include Enum.ExtendTable (M)
  end

  module Table = struct
    type elt = t

    (* Not necessarily an associative list: each symbol might be
       in multiple different sort buckets. *)
    type nonrec t = (t * Sort.t) list

    let empty = [];;

    let of_sets =
      List.concat_map
        ~f:(fun (set, sort) ->
            List.map ~f:(fun sym -> (sym, sort))
              (Set.to_list set)
          )
    ;;

    let add tbl sym sort = (sym, sort) :: tbl;;

    let remove tbl sym sort =
      MyList.exclude
        ~f:(Tuple2.equal ~eq1:String.equal ~eq2:Sort.equal (sym, sort))
        tbl
    ;;

    let set_of_sorts tbl sorts =
      tbl
      |> List.filter_map
        ~f:(fun (sym, sort) ->
            if Sort.Set.mem sorts sort then Some sym else None)
      |> Set.of_list
    ;;

    let set_of_sort tbl sort = set_of_sorts tbl (Sort.Set.singleton sort);;

    let set tbl = tbl |> List.map ~f:fst |> Set.of_list;;
  end

  let program_id_of sym =
    let open Option.Let_syntax in
    let%bind num_s = String.chop_prefix ~prefix:"P" sym in
    let%bind num   = Caml.int_of_string_opt num_s in
    Option.some_if (Int.is_non_negative num) num
  ;;

  let%expect_test "program_id_of: valid" =
    Sexp.output Out_channel.stdout [%sexp (program_id_of "P0" : int option)];
    [%expect {| (0) |}]
end
