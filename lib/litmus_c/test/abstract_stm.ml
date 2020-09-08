(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Src = Act_litmus_c
end

let%test_module "model" =
  ( module struct
    let test (stm : Src.Ast.Stm.t) : unit =
      Stdio.print_s
        [%sexp
          (Src.Abstract_stm.model stm : unit Fir.Statement.t Or_error.t)]

    let%expect_test "model explicit block" =
      test
        Src.Ast.(
          Stm.Compound
            [ `Stm
                (Stm.Expr
                   (Some
                      (Expr.Binary
                         ( Expr.Identifier (Ac.C_id.of_string "x")
                         , `Assign
                         , Expr.Identifier (Ac.C_id.of_string "y") )))) ]) ;
      [%expect
        {|
          (Ok
           (Flow
            ((header Explicit)
             (body
              ((statements
                ((Prim ()
                  (Assign
                   ((dst (Variable x)) (src (Expr (Address (Lvalue (Variable y))))))))))
               (metadata ())))))) |}]

    let%expect_test "model upwards for loop" =
      test
        Src.Ast.(
          Stm.For
            { init=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Assign
                     , Constant (Integer 0) ))
            ; cond=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Lt
                     , Constant (Integer 42) ))
            ; update=
                Some (Postfix (Identifier (Ac.C_id.of_string "x"), `Inc))
            ; body= Stm.Expr None }) ;
      [%expect
        {|
        (Ok
         (Flow
          ((header
            (For
             ((lvalue (Variable x)) (init_value (Constant (Int 0)))
              (cmp_value (Constant (Int 42))) (direction Up_exclusive))))
           (body ((statements ((Prim () Nop))) (metadata ())))))) |}]

    let%expect_test "model downwards not-equal for loop" =
      test
        Src.Ast.(
          Stm.For
            { init=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Assign
                     , Constant (Integer 53) ))
            ; cond=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Ne
                     , Constant (Integer 27) ))
            ; update=
                Some (Postfix (Identifier (Ac.C_id.of_string "x"), `Dec))
            ; body= Stm.Expr None }) ;
      [%expect
        {|
        (Ok
         (Flow
          ((header
            (For
             ((lvalue (Variable x)) (init_value (Constant (Int 53)))
              (cmp_value (Constant (Int 27))) (direction Down_exclusive))))
           (body ((statements ((Prim () Nop))) (metadata ())))))) |}]

    let%expect_test "model downwards-inclusive for loop" =
      test
        Src.Ast.(
          Stm.For
            { init=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Assign
                     , Constant (Integer 42) ))
            ; cond=
                Some
                  (Binary
                     ( Identifier (Ac.C_id.of_string "x")
                     , `Ge
                     , Constant (Integer 0) ))
            ; update=
                Some (Postfix (Identifier (Ac.C_id.of_string "x"), `Dec))
            ; body= Stm.Expr None }) ;
      [%expect
        {|
        (Ok
         (Flow
          ((header
            (For
             ((lvalue (Variable x)) (init_value (Constant (Int 42)))
              (cmp_value (Constant (Int 0))) (direction Down_inclusive))))
           (body ((statements ((Prim () Nop))) (metadata ())))))) |}]

    let%expect_test "model atomic_store_explicit" =
      test
        Src.Ast.(
          Stm.Expr
            (Some
               (Expr.Call
                  { func=
                      Identifier (Ac.C_id.of_string "atomic_store_explicit")
                  ; arguments=
                      [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                      ; Constant (Integer 42)
                      ; Identifier (Ac.C_id.of_string "memory_order_relaxed")
                      ] }))) ;
      [%expect
        {|
      (Ok
       (Prim ()
        (Atomic
         (Store
          ((src (Constant (Int 42))) (dst (Ref (Lvalue (Variable x))))
           (mo memory_order_relaxed)))))) |}]

    let%expect_test "model atomic cmpxchg" =
      test
        Act_litmus_c.Ast.(
          Stm.Expr
            (Some
               (Expr.Call
                  { func=
                      Identifier
                        (Ac.C_id.of_string
                           "atomic_compare_exchange_strong_explicit")
                  ; arguments=
                      [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                      ; Prefix (`Ref, Identifier (Ac.C_id.of_string "y"))
                      ; Constant (Integer 42)
                      ; Identifier (Ac.C_id.of_string "memory_order_relaxed")
                      ; Identifier (Ac.C_id.of_string "memory_order_relaxed")
                      ] }))) ;
      [%expect
        {|
      (Ok
       (Prim ()
        (Atomic
         (Cmpxchg
          ((obj (Ref (Lvalue (Variable x)))) (expected (Ref (Lvalue (Variable y))))
           (desired (Constant (Int 42))) (succ memory_order_relaxed)
           (fail memory_order_relaxed)))))) |}]
  end )
