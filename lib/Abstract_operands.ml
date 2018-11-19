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

open Core_kernel

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
  | `Location of Abstract_location.t
  ]
[@@deriving sexp]
;;

let pp_common_or_loc f = function
  | #common as c -> pp_common f c
  | `Location loc -> Abstract_location.pp f loc
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

let to_list : t -> any list = function
  | `None -> []
  | `Single x -> [x]
  | `Double (x, y) -> [x; y]
  | `Src_dst {Src_dst.src; dst} -> [src; (dst :> any)]
  | `Unknown | `Other | `Erroneous _ as x -> [(x :> any)]
;;

let is_part_unknown (x : t) : bool =
  let f = function
    | `Unknown -> true
    | `Location _ | `Int _ | `Symbol _
    | `Other | `Erroneous _ -> false
  in List.exists ~f (to_list x)
;;

let errors (x : t) : Error.t list =
  let f = function
    | `Erroneous x -> Some x
    | `Location _ | `Int _ | `Symbol _
    | `Other | `Unknown -> None
  in List.filter_map ~f (to_list x)
;;

let uses_immediate_heap_symbol operands ~syms =
  let f = function
    | `Symbol sym ->
      Abstract_symbol.(Table.mem syms ~sort:Sort.Heap sym)
    | `Location _ | `Int _
    | `Other | `Unknown | `Erroneous _ -> false
  in List.exists ~f (to_list operands)
;;

let%expect_test "uses_immediate_heap_symbol: src/dst positive" =
  let syms = Abstract_symbol.(
    Table.of_sets
      [ Set.of_list [ "foo"; "bar"; "baz" ], Sort.Heap
      ; Set.of_list [ "froz" ], Sort.Label
      ]
  )
  in
  let result = uses_immediate_heap_symbol ~syms
      (`Src_dst
         { src = `Symbol "foo"
         ; dst = `Location GeneralRegister
         })
  in
  Sexp.output_hum Out_channel.stdout [%sexp (result : bool)];
  [%expect {| true |}]
;;

let%expect_test "uses_immediate_heap_symbol: src/dst negative" =
  let syms = Abstract_symbol.(
      Table.of_sets
        [ Set.of_list [ "foo"; "bar"; "baz" ], Sort.Heap
        ; Set.of_list [ "froz" ], Sort.Label
      ]
    )
  in
  let result = uses_immediate_heap_symbol ~syms
      (`Src_dst
         { src = `Symbol "froz"
         ; dst = `Location GeneralRegister
         })
  in
  Sexp.output_hum Out_channel.stdout [%sexp (result : bool)];
  [%expect {| false |}]
;;

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

module Flag = Abstract_flag.None
