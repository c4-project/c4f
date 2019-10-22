(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Src = Act_c_mini

let%test_module "sift_decls" =
  ( module struct
    let%expect_test "sift_decls: mixed example" =
      let result =
        Or_error.(
          [`Decl "foo"; `Decl "bar"; `Ndecl "baz"; `Ndecl "barbaz"]
          |> Src.Convert.sift_decls
          >>| Tuple2.map_snd
                ~f:
                  (List.map ~f:(function
                    | `Decl _ ->
                        "DECL"
                    | `Ndecl x ->
                        x)))
      in
      Stdio.print_s
        [%sexp (result : (string list * string list) Or_error.t)] ;
      [%expect {| (Ok ((foo bar) (baz barbaz))) |}]
  end )

let%test_module "stm" =
  ( module struct
    let%expect_test "model atomic_store_explicit" =
      Stdio.print_s
        [%sexp
          ( Src.Convert.stm
              Act_c_lang.Ast.(
                Stm.Expr
                  (Some
                     (Expr.Call
                        { func=
                            Identifier
                              (Ac.C_id.of_string "atomic_store_explicit")
                        ; arguments=
                            [ Prefix
                                (`Ref, Identifier (Ac.C_id.of_string "x"))
                            ; Constant (Integer 42)
                            ; Identifier
                                (Ac.C_id.of_string "memory_order_relaxed")
                            ] })))
            : unit Src.Statement.t Or_error.t )] ;
      [%expect
        {|
      (Ok
       (Atomic_store
        ((src (Constant (Int 42))) (dst (Ref (Lvalue (Variable x))))
         (mo memory_order_relaxed)))) |}]

    let%expect_test "model atomic cmpxchg" =
      Stdio.print_s
        [%sexp
          ( Src.Convert.stm
              Act_c_lang.Ast.(
                Stm.Expr
                  (Some
                     (Expr.Call
                        { func=
                            Identifier
                              (Ac.C_id.of_string
                                 "atomic_compare_exchange_strong_explicit")
                        ; arguments=
                            [ Prefix
                                (`Ref, Identifier (Ac.C_id.of_string "x"))
                            ; Prefix
                                (`Ref, Identifier (Ac.C_id.of_string "y"))
                            ; Constant (Integer 42)
                            ; Identifier
                                (Ac.C_id.of_string "memory_order_relaxed")
                            ; Identifier
                                (Ac.C_id.of_string "memory_order_relaxed")
                            ] })))
            : unit Src.Statement.t Or_error.t )] ;
      [%expect
        {|
      (Ok
       (Atomic_cmpxchg
        ((obj (Ref (Lvalue (Variable x)))) (expected (Ref (Lvalue (Variable y))))
         (desired (Constant (Int 42))) (succ memory_order_relaxed)
         (fail memory_order_relaxed)))) |}]
  end )

let%test_module "expr" =
  ( module struct
    let%expect_test "model atomic_load_explicit" =
      Stdio.print_s
        [%sexp
          ( Src.Convert.expr
              Act_c_lang.Ast.(
                Expr.Call
                  { func=
                      Identifier (Ac.C_id.of_string "atomic_load_explicit")
                  ; arguments=
                      [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                      ; Identifier
                          (Ac.C_id.of_string "memory_order_seq_cst") ] })
            : Src.Expression.t Or_error.t )] ;
      [%expect
        {|
      (Ok
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_seq_cst)))) |}]
  end )