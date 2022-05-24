(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let dump : C4f_fir.Type.t list -> unit =
  List.iter ~f:(fun ty ->
      Stdio.printf "str: %s; sexp: " (C4f_fir.Type.to_string ty) ;
      print_s [%sexp (ty : C4f_fir.Type.t)] )

let%expect_test "bool: combinatoric" =
  dump
    C4f_fir.Type.
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
    C4f_fir.Type.
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
    (module C4f_fir.Type)
    ~f:
      C4f_fir.Type.(
        [%test_pred: t] ~here:[[%here]] (fun t ->
            basic_type_is t ~basic:(basic_type t) ))

let%test_module "check_atomic_non" =
  ( module struct
    let test (atomic : C4f_fir.Type.t) (non : C4f_fir.Type.t) : unit =
      let result = C4f_fir.Type.check_atomic_non ~atomic ~non in
      Stdio.print_s [%sexp (result : C4f_fir.Type.t Or_error.t)]

    let%expect_test "valid: atomic_int to int" =
      C4f_fir.Type.(test (int ~is_atomic:true ()) (int ())) ;
      [%expect {| (Ok int) |}]
  end )
