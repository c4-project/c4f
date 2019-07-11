(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%expect_test "int_type: combinatoric" =
  print_s
    [%sexp
      ( Act_c_mini.Type.
          [ int_type ~is_atomic:false ~is_pointer:false
          ; int_type ~is_atomic:false ~is_pointer:true
          ; int_type ~is_atomic:true ~is_pointer:false
          ; int_type ~is_atomic:true ~is_pointer:true ]
        : Act_c_mini.Type.t list )] ;
  [%expect
    {| ((Normal int) (Pointer_to int) (Normal atomic_int) (Pointer_to atomic_int)) |}]
