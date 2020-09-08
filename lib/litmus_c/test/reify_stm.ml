(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
                (Assign.make
                   ~lvalue:(Lvalue.of_variable_str_exn "x")
                   ~rvalue:Expression.truth)
            ; A.(construct (Statement.prim' @> Prim_statement.assign))
                (Assign.make
                   ~lvalue:(Lvalue.of_variable_str_exn "y")
                   ~rvalue:Expression.falsehood) ]
          ())

    let%expect_test "upwards for loop" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop
                ~control:
                  (For.make
                     ~lvalue:(Lvalue.of_variable_str_exn "x")
                     ~init_value:(Expression.int_lit 0)
                     ~cmp_value:(Expression.int_lit 42)
                     ~direction:Up_exclusive)
                ~body:test_body)) ;
      [%expect {| for (x = 0; x < 42; x++) { x = true; y = false; } |}]

    let%expect_test "downwards-inclusive for loop" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop
                ~control:
                  (For.make
                     ~lvalue:(Lvalue.of_variable_str_exn "x")
                     ~init_value:(Expression.int_lit 53)
                     ~cmp_value:(Expression.int_lit 27)
                     ~direction:Down_inclusive)
                ~body:test_body)) ;
      [%expect {| for (x = 53; x >= 27; x--) { x = true; y = false; } |}]

    let%expect_test "upwards-inclusive for loop with pointers" =
      test
        Fir.(
          A.construct Statement.flow
            Flow_block.(
              for_loop
                ~control:
                  (For.make
                     ~lvalue:(Lvalue.deref (Lvalue.of_variable_str_exn "x"))
                     ~init_value:(Expression.int_lit 0)
                     ~cmp_value:(Expression.int_lit 100)
                     ~direction:Up_inclusive)
                ~body:test_body)) ;
      [%expect
        {| for (*x = 0; *x <= 100; (*x)++) { x = true; y = false; } |}]

    let%expect_test "explicit block" =
      test Fir.(A.construct Statement.flow (Flow_block.explicit test_body)) ;
      [%expect {| { x = true; y = false; } |}]

    let%expect_test "implicit block" =
      test Fir.(A.construct Statement.flow (Flow_block.implicit test_body)) ;
      [%expect {| x = true; y = false; |}]
  end )
