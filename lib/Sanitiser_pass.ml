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

open Utils

module M = struct
  type t =
    | LangHooks
    | MangleSymbols
    | RemoveBoundaries
    | RemoveLitmus
    | RemoveUseless
    | SimplifyLitmus
    | Warn
  [@@deriving enum, sexp]

  let table =
    [ LangHooks       , "lang-hooks"
    ; MangleSymbols   , "mangle-symbols"
    ; RemoveBoundaries, "remove-boundaries"
    ; RemoveLitmus    , "remove-litmus"
    ; RemoveUseless   , "remove-useless"
    ; SimplifyLitmus  , "simplify-litmus"
    ; Warn            , "warn"
    ]
end

include M
include Enum.Extend_table (M)

let explain = Set.of_list [ RemoveUseless ]

let%expect_test "all passes accounted for" =
  Format.printf "@[<v>%a@]@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp)
    (all_list ());
  [%expect {|
    lang-hooks
    mangle-symbols
    remove-boundaries
    remove-litmus
    remove-useless
    simplify-litmus
    warn |}]
;;
