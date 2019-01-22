(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base
open Utils

type t =
  [ `Assembly
  | `C
  | `C_litmus
  ]
;;

type t_or_infer =
  [ t
  | `Infer
  ]
;;

let file_type_is (src : Io.In_source.t) (expected : string) : bool =
  Option.exists (Io.In_source.file_type src) ~f:(String.equal expected)
;;

let is_c (src : Io.In_source.t)
  : [> `C | `Infer] -> bool = function
  | `C     -> true
  | `Infer -> file_type_is src "c"
  | _      -> false
;;

let%expect_test "is_c: infer on untyped stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ()) `Infer);
  [%expect {| false |}]
;;

let%expect_test "is_c: infer on C-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"c" ()) `Infer);
  [%expect {| true |}]
;;

let%expect_test "is_c: infer on asm-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"s" ()) `Infer);
  [%expect {| false |}]
;;

let%expect_test "is_c: infer on C-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.c")) `Infer);
  [%expect {| true |}]
;;

let%expect_test "is_c: infer on asm-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.s")) `Infer);
  [%expect {| false |}]
;;

let%expect_test "is_c: override C on untyped stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ()) `C);
  [%expect {| true |}]
;;

let%expect_test "is_c: override C on C-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"c" ()) `C);
  [%expect {| true |}]
;;

let%expect_test "is_c: override C on asm-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"s" ()) `C);
  [%expect {| true |}]
;;

let%expect_test "is_c: override C on C-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.c")) `C);
  [%expect {| true |}]
;;

let%expect_test "is_c: override C on asm-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.s")) `C);
  [%expect {| true |}]
;;

let%expect_test "is_c: override non-C on untyped stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ()) `Assembly);
  [%expect {| false |}]
;;

let%expect_test "is_c: override non-C on C-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"c" ()) `Assembly);
  [%expect {| false |}]
;;

let%expect_test "is_c: override non-C on asm-typed stdin" =
  Stdio.printf "%b\n" (is_c (Io.In_source.stdin ~file_type:"s" ()) `Assembly);
  [%expect {| false |}]
;;

let%expect_test "is_c: override non-C on C-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.c")) `Assembly);
  [%expect {| false |}]
;;

let%expect_test "is_c: override non-C on asm-typed file" =
  Stdio.printf "%b\n" (is_c (Io.In_source.file (Fpath.v "hello.s")) `Assembly);
  [%expect {| false |}]
;;

let is_c_litmus (src : Io.In_source.t)
  : [> `C_litmus | `Infer] -> bool = function
  | `C_litmus -> true
  | `Infer -> file_type_is src "litmus"
  | _ -> false
;;

let delitmusified : t_or_infer -> t_or_infer = function
  | `C_litmus -> `C
  | `Assembly | `C | `Infer as x -> x
;;
