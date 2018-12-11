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
open Utils

module M = struct
  type t =
    [ `Escape_symbols
    | `Language_hooks
    | `Remove_boundaries
    | `Remove_litmus
    | `Remove_useless
    | `Simplify_deref_chains
    | `Simplify_litmus
    | `Unmangle_symbols
    | `Warn
    ]
  [@@deriving enum, sexp]

  let table =
    [ `Escape_symbols       , "escape-symbols"
    ; `Language_hooks       , "language-hooks"
    ; `Remove_boundaries    , "remove-boundaries"
    ; `Remove_litmus        , "remove-litmus"
    ; `Remove_useless       , "remove-useless"
    ; `Simplify_deref_chains, "simplify-deref-chains"
    ; `Simplify_litmus      , "simplify-litmus"
    ; `Unmangle_symbols     , "unmangle-symbols"
    ; `Warn                 , "warn"
    ]
end

include M
include Enum.Extend_table (M)

let%expect_test "all passes accounted for" =
  Format.printf "@[<v>%a@]@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp)
    (all_list ());
  [%expect {|
    escape-symbols
    language-hooks
    remove-boundaries
    remove-litmus
    remove-useless
    simplify-deref-chains
    simplify-litmus
    unmangle-symbols
    warn |}]
;;

let all_lazy = lazy (all_set ())

let explain = Set.of_list [ `Remove_useless ]
let standard = all_set ()

module Selector = struct
  type elt = M.t

  type category =
    [ `Standard
    | `Explain
    ]
  [@@deriving sexp]
  ;;

  type t =
    [ M.t
    | category
    | `Default
    ]
  [@@deriving sexp]
  ;;

  let eval (default : Set.t) : t -> Set.t = function
    | #elt as pass -> Set.singleton pass
    | `Standard -> standard
    | `Explain  -> explain
    | `Default  -> default
  ;;

  let eval_b pred ~default =
    Blang.eval_set ~universe:all_lazy (eval default) pred

  let%expect_test "eval_b: standard and not explain" =
    let blang = Blang.O.((base `Standard) && not (base `Explain)) in
    Sexp.output_hum Out_channel.stdout
      [%sexp (eval_b blang ~default:Set.empty : Set.t)];
    [%expect {|
      (escape-symbols language-hooks remove-boundaries remove-litmus
       simplify-deref-chains simplify-litmus unmangle-symbols warn) |}]
  ;;
end
