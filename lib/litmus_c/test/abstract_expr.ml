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

let test (expr : Src.Ast.Expr.t) : unit =
  Stdio.print_s
    [%sexp (Src.Abstract_expr.model expr : Fir.Expression.t Or_error.t)]

let%expect_test "model basic logical expression" =
  test
    Act_litmus_c.Ast.(
      Expr.(
        Binary
          ( Identifier (Ac.C_id.of_string "true")
          , `Land
          , Binary
              ( Identifier (Ac.C_id.of_string "false")
              , `Lor
              , Identifier (Ac.C_id.of_string "foo") ) ))) ;
  [%expect
    {|
          (Ok
           (Bop (Logical And) (Constant (Bool true))
            (Bop (Logical Or) (Constant (Bool false))
             (Address (Lvalue (Variable foo)))))) |}]

let%expect_test "model atomic_load_explicit" =
  test
    Act_litmus_c.Ast.(
      Expr.Call
        { func= Identifier (Ac.C_id.of_string "atomic_load_explicit")
        ; arguments=
            [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
            ; Identifier (Ac.C_id.of_string "memory_order_seq_cst") ] }) ;
  [%expect
    {|
      (Ok
       (Atomic
        (Load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_seq_cst))))) |}]
