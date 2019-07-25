(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%expect_test "bool: combinatoric" =
  print_s
    [%sexp
      ( Act_c_mini.Type.
          [ bool ~atomic:false ~pointer:false ()
          ; bool ~atomic:false ~pointer:true ()
          ; bool ~atomic:true ~pointer:false ()
          ; bool ~atomic:true ~pointer:true () ]
        : Act_c_mini.Type.t list )] ;
  [%expect {| (bool bool* atomic_bool atomic_bool*) |}]

let%expect_test "int: combinatoric" =
  print_s
    [%sexp
      ( Act_c_mini.Type.
          [ int ~atomic:false ~pointer:false ()
          ; int ~atomic:false ~pointer:true ()
          ; int ~atomic:true ~pointer:false ()
          ; int ~atomic:true ~pointer:true () ]
        : Act_c_mini.Type.t list )] ;
  [%expect {| (int int* atomic_int atomic_int*) |}]

let%test_unit "basic_type_is compatibility with basic_type" =
  Base_quickcheck.Test.run_exn
    (module Act_c_mini.Type)
    ~f:
      Act_c_mini.Type.(
        [%test_pred: t] ~here:[[%here]] (fun t ->
            basic_type_is t ~basic:(basic_type t)))
