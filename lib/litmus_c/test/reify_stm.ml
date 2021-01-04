(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor
  module Fir = Act_fir
  module Src = Act_litmus_c
end

let%test_module "reify/pp" =
  ( module struct
    let test (stm : unit Fir.Statement.t) : unit =
      Fmt.pr "@[%a@]@." Src.Reify_stm.pp stm

    let test_body : (unit, unit Fir.Statement.t) Fir.Block.t =
      Fir.(
        Block.make ~metadata:()
          ~statements:
            [ A.(construct (Statement.prim' @> Prim_statement.assign))
                Assign.(Lvalue.of_variable_str_exn "x" @= Expression.truth)
            ; A.(construct (Statement.prim' @> Prim_statement.assign))
                Assign.(
                  Lvalue.of_variable_str_exn "y" @= Expression.falsehood) ]
          ())

    let%expect_test "upwards for loop" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop_simple test_body
                ~control:
                  { For.Simple.lvalue= Lvalue.of_variable_str_exn "x"
                  ; init_value= Expression.int_lit 0
                  ; cmp_value= Expression.int_lit 42
                  ; direction= Up Exclusive })) ;
      [%expect {| for (x = 0; x < 42; x++) { x = true; y = false; } |}]

    let%expect_test "downwards-inclusive for loop" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop_simple test_body
                ~control:
                  { For.Simple.lvalue= Lvalue.of_variable_str_exn "x"
                  ; init_value= Expression.int_lit 53
                  ; cmp_value= Expression.int_lit 27
                  ; direction= Down Inclusive })) ;
      [%expect {| for (x = 53; x >= 27; x--) { x = true; y = false; } |}]

    let%expect_test "infinite for loop" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(for_loop test_body ~control:(For.make ()))) ;
      [%expect {| for (; ; ) { x = true; y = false; } |}]

    let%expect_test "upwards-inclusive for loop with pointers" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop_simple test_body
                ~control:
                  { For.Simple.lvalue=
                      A.construct Lvalue.deref
                        (Lvalue.of_variable_str_exn "x")
                  ; init_value= Expression.int_lit 0
                  ; cmp_value= Expression.int_lit 100
                  ; direction= Up Inclusive })) ;
      [%expect
        {| for (*x = 0; *x <= 100; (*x)++) { x = true; y = false; } |}]

    let%expect_test "explicit block" =
      test Fir.(A.construct Statement.flow (Flow_block.explicit test_body)) ;
      [%expect {| { x = true; y = false; } |}]

    let%expect_test "implicit block" =
      test Fir.(A.construct Statement.flow (Flow_block.implicit test_body)) ;
      [%expect {| x = true; y = false; |}]

    let%expect_test "increment of a variable" =
      test
        Fir.(
          A.(
            construct
              (Statement.prim' @> Prim_statement.assign)
              (Assign.make ~dst:(Lvalue.of_variable_str_exn "x") ~src:Inc))) ;
      [%expect {| x++; |}]

    let%expect_test "increment of a pointer" =
      test
        Fir.(
          A.(
            construct
              (Statement.prim' @> Prim_statement.assign)
              (Assign.make
                 ~dst:Lvalue.(construct deref (of_variable_str_exn "x"))
                 ~src:Inc))) ;
      [%expect {| (*x)++; |}]
  end )
