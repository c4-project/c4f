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

let%test_module "Eval" =
  ( module struct
    let test (e : Src.Expression.t)
        ~(env : Src.Address.t -> Src.Constant.t Or_error.t) : unit =
      let k = Src.Expression.Eval.as_constant e ~env in
      print_s [%sexp (k : Src.Constant.t Or_error.t)]

    let test_pure : Src.Expression.t -> unit =
      test ~env:Src.Expression.Eval.empty_env

    let test_mod (e : Src.Expression.t) : unit =
      test e
        ~env:(Src.Address.eval_on_env (Lazy.force Env.det_known_value_mod))

    let invalid_expr : Src.Expression.t =
      Src.Expression.(eq (int_lit 27) (bool_lit false))

    let%expect_test "example true 'pure' Boolean expression" =
      test_pure
        Src.Expression.(
          l_and
            (eq (int_lit 27) (int_lit 27))
            (l_or (bool_lit true) (bool_lit false))) ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "example false 'pure' Boolean expression" =
      test_pure
        Src.Expression.(
          l_and
            (eq (int_lit 27) (int_lit 27))
            (l_and (bool_lit true) (bool_lit false))) ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "example short-circuiting true 'pure' Boolean expression"
        =
      test_pure
        Src.Expression.(
          l_or (l_or (bool_lit true) (bool_lit false)) invalid_expr) ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "example short-circuiting false 'pure' Boolean \
                     expression" =
      test_pure
        Src.Expression.(l_and (eq (int_lit 27) (int_lit 53)) invalid_expr) ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "example invalid expression" =
      test_pure invalid_expr ;
      [%expect
        {|
      (Error
       ("eq: types of constants are incompatible" (left (Int 27))
        (right (Bool false)))) |}]

    let%expect_test "example true env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          l_and
            (eq
               (lvalue (Src.Lvalue.variable (Act_common.C_id.of_string "x")))
               (int_lit 27))
            (l_or (bool_lit true) (bool_lit false))) ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "example false env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          l_and
            (l_or (bool_lit true) (bool_lit false))
            (eq
               (lvalue (Src.Lvalue.variable (Act_common.C_id.of_string "y")))
               (int_lit 27))) ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "example invalid env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          eq
            (lvalue (Src.Lvalue.variable (Act_common.C_id.of_string "a")))
            (int_lit 27)) ;
      [%expect
        {| (Error ("Variable not found in typing environment." (id a))) |}]

    let%expect_test "example atomic load" =
      let e = Lazy.force Env.test_env_mod in
      test_mod
        Src.(
          Expression.(
            atomic_load
              (Atomic_load.make
                 ~src:
                   (Or_error.ok_exn
                      (Address.of_id_in_env e
                         ~id:(Act_common.C_id.of_string "y")))
                 ~mo:Act_c_mini.Mem_order.Seq_cst))) ;
      [%expect {| (Ok (Int 53)) |}]
  end )
