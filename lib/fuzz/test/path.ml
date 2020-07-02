(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%test_module "s-expression serialisation" =
  ( module struct
    open struct
      module P = Act_fuzz.Path
    end

    let test (x : P.Test.t) : unit = print_s [%sexp (x : P.Test.t)]

    let%expect_test "this-statement example" =
      test
        P.(
          Test.in_thread 0 @@ Thread.in_stms @@ Stms.in_stm 2 @@ Stm.in_if
          @@ If.in_branch true @@ Stms.in_stm 5 @@ Stm.this_stm) ;
      [%expect
        {|
          (In_thread 0
           (In_stms (In_stm 2 (In_if (In_block (true (In_stm 5 This_stm))))))) |}]

    let%expect_test "insert-statement example" =
      test P.(Test.in_thread 3 @@ Thread.in_stms @@ Stms.insert 0) ;
      [%expect {| (In_thread 3 (In_stms (Insert 0))) |}]

    let%expect_test "if-statement conditional example" =
      test
        P.(
          Test.in_thread 2 @@ Thread.in_stms @@ Stms.in_stm 9 @@ Stm.in_if
          @@ If.this_cond) ;
      [%expect {| (In_thread 2 (In_stms (In_stm 9 (In_if This_cond)))) |}]
  end )
