(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = C4f_fir

let%test_module "Eval" =
  ( module struct
    let test (e : Src.Expression.t) ~(env : Src.Heap.t) : unit =
      let k = Src.Expression_eval.as_constant e ~env in
      print_s [%sexp (k : Src.Constant.t Or_error.t)]

    let test_pure : Src.Expression.t -> unit = test ~env:(Src.Heap.empty ())

    let test_mod (e : Src.Expression.t) : unit =
      let src = Src.Address.eval_on_env ~env:(Lazy.force Env.test_env) in
      let env = Src.Heap.make src in
      test e ~env

    let invalid_expr : Src.Expression.t =
      Src.Expression.(eq (int_lit 27) falsehood)

    let%expect_test "example true 'pure' Boolean expression" =
      test_pure
        Src.Expression.(
          l_and (eq (int_lit 27) (int_lit 27)) (l_or truth falsehood) ) ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "example false 'pure' Boolean expression" =
      test_pure
        Src.Expression.(
          l_and (eq (int_lit 27) (int_lit 27)) (l_and truth falsehood) ) ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "example short-circuiting true 'pure' Boolean expression"
        =
      test_pure Src.Expression.(l_or (l_or truth falsehood) invalid_expr) ;
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
       ("types of relational inputs don't match" (left (Int 27))
        (right (Bool false)))) |}]

    let%expect_test "example true env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          l_and
            (eq (lvalue (Src.Lvalue.of_variable_str_exn "x")) (int_lit 27))
            (l_or truth falsehood) ) ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "example false env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          l_and (l_or truth falsehood)
            (eq (lvalue (Src.Lvalue.of_variable_str_exn "y")) (int_lit 27)) ) ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "example invalid env-sensitive Boolean expression" =
      test_mod
        Src.Expression.(
          eq (lvalue (Src.Lvalue.of_variable_str_exn "a")) (int_lit 27) ) ;
      [%expect
        {| (Error ("Variable not found in typing environment." (id a))) |}]

    let var_addr (env : Src.Env.t) (x : string) : Src.Address.t =
      Or_error.ok_exn
        (Src.Address.of_id_in_env env ~id:(C4f_common.C_id.of_string x))

    let%expect_test "example atomic load" =
      let e = Lazy.force Env.test_env in
      test_mod
        Src.(
          Expression.(
            atomic_load
              (Atomic_load.make ~src:(var_addr e "y")
                 ~mo:C4f_fir.Mem_order.Seq_cst ) ) ) ;
      [%expect {| (Ok (Int 53)) |}]

    let%expect_test "example atomic load" =
      let e = Lazy.force Env.test_env in
      test_mod
        Src.(
          Expression.(
            atomic_load
              (Atomic_load.make ~src:(var_addr e "y")
                 ~mo:C4f_fir.Mem_order.Seq_cst ) ) ) ;
      [%expect {| (Ok (Int 53)) |}]

    let%expect_test "example atomic fetches" =
      let e = Lazy.force Env.test_env in
      test_mod
        Src.(
          Expression.(
            sub
              (atomic_fetch
                 (Atomic_fetch.make ~obj:(var_addr e "y") ~arg:(int_lit 1)
                    ~mo:C4f_fir.Mem_order.Seq_cst ~op:`Sub ) )
              (atomic_fetch
                 (Atomic_fetch.make ~obj:(var_addr e "y") ~arg:(int_lit 1)
                    ~mo:C4f_fir.Mem_order.Seq_cst ~op:`Add ) ) ) ) ;
      [%expect {| (Ok (Int 1)) |}]
  end )
