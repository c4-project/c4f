(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = C4f_common
  module Fir = C4f_fir
  module Src = C4f_litmus_c
end

let test (expr : Src.Ast.Expr.t) : unit =
  Stdio.print_s
    [%sexp (Src.Abstract_expr.model expr : Fir.Expression.t Or_error.t)]

let%expect_test "model basic logical expression" =
  test
    C4f_litmus_c.Ast.(
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
    C4f_litmus_c.Ast.(
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
