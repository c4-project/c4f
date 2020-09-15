(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_fir

let%test_module "depended_upon_idents" =
  ( module struct
    let test (e : Src.Expression.t) : unit =
      let xs =
        Accessor.to_list Src.Expression_traverse.depended_upon_idents e
      in
      print_s [%sexp (xs : Act_common.C_id.t list)]

    (* This test case will likely expand if we make dependency more
       sophisticated. *)

    let%expect_test "multiple nested depended-upon idents" =
      test
        Src.(
          Expression.(
            add
              (variable (Act_common.C_id.of_string "foo"))
              (sub
                 (variable (Act_common.C_id.of_string "bar"))
                 (lvalue
                    Lvalue.(
                      Accessor.construct deref (of_variable_str_exn "baz")))))) ;
      [%expect {| (foo bar baz) |}]
  end )
