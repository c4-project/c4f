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
                ~body:(Block.make ~metadata:() ()))) ;
      [%expect {| for (x = 0; x < 42; x++) {  } |}]

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
                ~body:(Block.make ~metadata:() ()))) ;
      [%expect {| for (x = 53; x >= 27; x--) {  } |}]

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
                ~body:(Block.make ~metadata:() ()))) ;
      [%expect {| for (*x = 0; *x <= 100; (*x)++) {  } |}]
  end )
