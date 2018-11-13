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

module type S = sig
  type t [@@deriving sexp]
  include Pretty_printer.S with type t := t
end

module type S_enum = sig
  include S
  include Enum.ExtensionTable with type t := t
end

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
  [@@deriving sexp]
  ;;

  let pp f = function
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
  [@@deriving sexp]

  let pp f = function
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
  type common =
    [ `Erroneous of Error.t
    | `Other
    | `Unknown
    ]
  [@@deriving sexp]
  ;;

  let pp_common f = function
    | `Erroneous _ -> String.pp f "<invalid operands>"
    | `Other       -> String.pp f "other"
    | `Unknown     -> String.pp f "??"
  ;;

  type common_or_loc =
    [ common
    | `Location of Location.t
    ]
  [@@deriving sexp]
  ;;

  let pp_common_or_loc f = function
    | #common as c -> pp_common f c
    | `Location loc -> Location.pp f loc
  ;;

  type src =
    [ common_or_loc
    | `Int of int
    | `Symbol of string
    ]
  [@@deriving sexp]
  ;;

  let pp_src f = function
    | #common_or_loc as c -> pp_common_or_loc f c
    | `Int k -> Format.fprintf f "@[<h>$%d@]" k
    | `Symbol s -> Format.fprintf f "@[<h>sym:%s@]" s
  ;;

  type dst = common_or_loc
  [@@deriving sexp]
  ;;

  let pp_dst = pp_common_or_loc

  type any = src
  [@@deriving sexp]
  ;;

  let pp_any = pp_src

  type t =
    [ common
    | `None
    | `Single of any
    | `Double of any * any
    | `Src_dst of (src, dst) Src_dst.t
    ]
  [@@deriving sexp]

  let rec is_part_unknown' = function
    | `Unknown -> true
    | `Single x -> is_part_unknown' (x :> [ t | any ])
    | `Double (x, y) ->
      is_part_unknown' (x :> [ t | any ])
      || is_part_unknown' (y :> [ t | any ])
    | `Src_dst {Src_dst.src; dst} ->
      is_part_unknown' (src :> [ t | any ])
      || is_part_unknown' (dst :> [t | any ])
    | `Location _ | `Int _ | `Symbol _
    | `None | `Other | `Erroneous _ -> false
  ;;
  let is_part_unknown (x : t) : bool = is_part_unknown' (x :> [ t | any ])

  let rec errors' = function
    | `Erroneous x -> [x]
    | `Single x -> errors' (x :> [ t | any ])
    | `Double (x, y) ->
      errors' (x :> [ t | any ]) @ errors' (y :> [ t | any ])
    | `Src_dst {Src_dst.src; dst} ->
      errors' (src :> [ t | any ]) @ errors' (dst :> [t | any ])
    | `Location _ | `Int _ | `Symbol _
    | `None | `Other | `Unknown -> []
  ;;
  let errors (x : t) : Error.t list = errors' (x :> [ t | any ])

  let single operand = `Single operand
  let double op1 op2 = `Double (op1, op2)

  let src_dst ~src ~dst = `Src_dst {Src_dst.src; dst}
  let is_src_dst : t -> bool = function
    | `Src_dst _ -> true
    | `None | `Single _ | `Double _
    | `Other | `Erroneous _ | `Unknown -> false
  ;;

  let pp f = function
    | #common as c -> pp_common f c
    | `None -> String.pp f "none"
    | `Single x -> pp_any f x
    | `Double (op1, op2) ->
      Format.fprintf f "@[%a,@ %a@]"
        pp_any op1
        pp_any op2
    | `Src_dst { Src_dst.src; dst} ->
      Format.fprintf f "@[%a@ ->@ %a@]"
        pp_src src
        pp_dst dst
  ;;
end

module Symbol = struct
  type t = string

  module Set = struct
    module S = Set.Make (String)
    include S
    include My_set.Extend (S)
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
      My_list.exclude
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
