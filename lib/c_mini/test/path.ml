(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Stdio

let%test_module "s-expression serialisation" = (module struct
  open Act_c_mini.Path_shapes

  let test (x : program) : unit = print_s [%sexp (x : program)]

  let%expect_test "this-statement example" =
    test
      (in_func 0
         (in_stms
           (in_stm 2
              (in_if
                 (in_block true
                    (in_stm 5
                       this_stm))))));
    [%expect {| (In_func 0 (In_stms (In_stm 2 (In_if (In_block true (In_stm 5 This_stm)))))) |}]

  let%expect_test "insert-statement example" =
    test
      (in_func 3
         (in_stms
            (insert 0)));
    [%expect {| (In_func 3 (In_stms (Insert 0))) |}]

  let%expect_test "if-statement conditional example" =
    test
      (in_func 2
         (in_stms
            (in_stm 9
               (in_if this_cond))));
    [%expect {| (In_func 2 (In_stms (In_stm 9 (In_if This_cond)))) |}]
end)
