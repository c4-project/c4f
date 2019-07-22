(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

module Src = Act_c_mini

let%test_module "Eval" = (module struct

  let test_pure (e : Src.Expression.t) : unit =
    let k = Src.Expression.Eval.(as_constant e ~env:empty_env) in
    print_s [%sexp (k : Src.Constant.t Or_error.t)]

  let invalid_expr : Src.Expression.t =
    Src.Expression.(eq (int_lit 27) (bool_lit false))

  let%expect_test "example true 'pure' Boolean expression" =
    test_pure
      Src.Expression.(
        l_and
          (eq (int_lit 27) (int_lit 27))
          (l_or (bool_lit true) (bool_lit false))
      );
    [%expect {| (Ok (Bool true)) |}]

  let%expect_test "example false 'pure' Boolean expression" =
    test_pure
      Src.Expression.(
        l_and
          (eq (int_lit 27) (int_lit 27))
          (l_and (bool_lit true) (bool_lit false))
      );
    [%expect {| (Ok (Bool false)) |}]

  let%expect_test "example short-circuiting true 'pure' Boolean expression" =
    test_pure
      Src.Expression.(
        l_or
          (l_or (bool_lit true) (bool_lit false))
          invalid_expr
      );
    [%expect {| (Ok (Bool true)) |}]

  let%expect_test "example short-circuiting false 'pure' Boolean expression" =
    test_pure
      Src.Expression.(
        l_and
          (eq (int_lit 27) (int_lit 53))
          invalid_expr
      );
    [%expect {| (Ok (Bool false)) |}]

  let%expect_test "example invalid expression" =
    test_pure invalid_expr;
    [%expect {|
      (Error
       ("eq: types of constants are incompatible" (left (Int 27))
        (right (Bool false)))) |}]
end)
