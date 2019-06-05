(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Stdio
module Ac = Act_common
module Aux = Act_asm.Litmusifier_aux

let%test_module "making locations from init" =
  ( module struct
    type pc = int Act_litmus.Ast_base.Postcondition.t

    let test ?(postcondition : pc option)
        (init : (Ac.C_id.t, int) List.Assoc.t) : unit =
      let locs = Aux.make_locations ~init ?postcondition () in
      print_s [%sexp (locs : Ac.C_id.t list)]

    let%expect_test "test init" =
      test
        Ac.C_id.
          [ (of_string "foo", 42)
          ; (of_string "bar", 27)
          ; (of_string "baz", 53) ] ;
      [%expect {| (bar baz foo) |}]

    let%expect_test "test init with postcondition mentioning global variable"
        =
      test
        ~postcondition:
          Act_litmus.Ast_base.(
            Postcondition.(
              make ~quantifier:`Exists
                ~predicate:
                  Pred_elt.(
                    Elt (Act_common.Litmus_id.of_string "barbaz" ==? 6))))
        Ac.C_id.[(of_string "foo", 42); (of_string "bar", 27)] ;
      [%expect {| (bar barbaz foo) |}]
  end )

let%test_module "making locations from variable map" =
  ( module struct
    let test (c_variables : Ac.C_variables.Map.t) : unit =
      let locs = Aux.make_locations ~init:[] ~c_variables () in
      print_s [%sexp (locs : Ac.C_id.t list)]

    let test_heap_symbols : Act_abstract.Symbol.Set.t =
      Act_abstract.Symbol.Set.of_list ["foo"; "barbaz"; "splink"]

    let test_global_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.of_single_scope_map
        ~scope:Ac.C_variables.Scope.Global
        Ac.C_id.(
          Map.of_alist_exn
            [ (of_string "foo", Some 42)
            ; (of_string "bar", Some 27)
            ; (of_string "barbaz", None)
            ; (of_string "blep", Some 63) ])

    let test_local_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.of_single_scope_map
        ~scope:Ac.C_variables.Scope.Local
        Ac.C_id.(
          Map.of_alist_exn
            [ (of_string "burble", Some 99)
            ; (of_string "splink", None)
            ; (of_string "herp", Some 21) ])

    let test_cvars : Ac.C_variables.Map.t =
      Ac.C_variables.Map.merge test_global_cvars test_local_cvars

    let%expect_test "unfiltered example" =
      test test_cvars ; [%expect {| (bar barbaz blep foo) |}]

    let filtered_cvars : Ac.C_variables.Map.t =
      Aux.live_symbols_only ~heap_symbols:test_heap_symbols test_cvars

    let%expect_test "filtered example" =
      test filtered_cvars ; [%expect {| (barbaz foo) |}]
  end )
