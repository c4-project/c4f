(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let dump : Act_c_mini.Type.t list -> unit =
  List.iter ~f:(fun ty ->
      Stdio.printf "str: %s; sexp: " (Act_c_mini.Type.to_string ty) ;
      print_s [%sexp (ty : Act_c_mini.Type.t)])

let%expect_test "bool: combinatoric" =
  dump
    Act_c_mini.Type.
      [ bool ~is_volatile:false ~is_atomic:false ~is_pointer:false ()
      ; bool ~is_volatile:false ~is_atomic:false ~is_pointer:true ()
      ; bool ~is_volatile:false ~is_atomic:true ~is_pointer:false ()
      ; bool ~is_volatile:false ~is_atomic:true ~is_pointer:true ()
      ; bool ~is_volatile:true ~is_atomic:false ~is_pointer:false ()
      ; bool ~is_volatile:true ~is_atomic:false ~is_pointer:true ()
      ; bool ~is_volatile:true ~is_atomic:true ~is_pointer:false ()
      ; bool ~is_volatile:true ~is_atomic:true ~is_pointer:true () ] ;
  [%expect
    {|
    str: bool; sexp: bool
    str: bool*; sexp: bool*
    str: atomic_bool; sexp: atomic_bool
    str: atomic_bool*; sexp: atomic_bool*
    str: volatile bool; sexp: "volatile bool"
    str: volatile bool*; sexp: "volatile bool*"
    str: volatile atomic_bool; sexp: "volatile atomic_bool"
    str: volatile atomic_bool*; sexp: "volatile atomic_bool*" |}]

let%expect_test "int: combinatoric" =
  dump
    Act_c_mini.Type.
      [ int ~is_volatile:false ~is_atomic:false ~is_pointer:false ()
      ; int ~is_volatile:false ~is_atomic:false ~is_pointer:true ()
      ; int ~is_volatile:false ~is_atomic:true ~is_pointer:false ()
      ; int ~is_volatile:false ~is_atomic:true ~is_pointer:true ()
      ; int ~is_volatile:true ~is_atomic:false ~is_pointer:false ()
      ; int ~is_volatile:true ~is_atomic:false ~is_pointer:true ()
      ; int ~is_volatile:true ~is_atomic:true ~is_pointer:false ()
      ; int ~is_volatile:true ~is_atomic:true ~is_pointer:true () ] ;
  [%expect
    {|
    str: int; sexp: int
    str: int*; sexp: int*
    str: atomic_int; sexp: atomic_int
    str: atomic_int*; sexp: atomic_int*
    str: volatile int; sexp: "volatile int"
    str: volatile int*; sexp: "volatile int*"
    str: volatile atomic_int; sexp: "volatile atomic_int"
    str: volatile atomic_int*; sexp: "volatile atomic_int*" |}]

let%test_unit "basic_type_is compatibility with basic_type" =
  Base_quickcheck.Test.run_exn
    (module Act_c_mini.Type)
    ~f:
      Act_c_mini.Type.(
        [%test_pred: t] ~here:[[%here]] (fun t ->
            basic_type_is t ~basic:(basic_type t)))

let%test_module "check_atomic_non" =
  ( module struct
    let test (atomic : Act_c_mini.Type.t) (non : Act_c_mini.Type.t) : unit =
      let result = Act_c_mini.Type.check_atomic_non ~atomic ~non in
      Stdio.print_s [%sexp (result : Act_c_mini.Type.t Or_error.t)]

    let%expect_test "valid: atomic_int to int" =
      Act_c_mini.Type.(test (int ~is_atomic:true ()) (int ())) ;
      [%expect {| (Ok int) |}]
  end )
