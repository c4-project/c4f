(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Act_common.File_type
module Au = Act_utils

let%test_module "is_c" =
  ( module struct
    let%expect_test "is_c: infer on untyped stdin" =
      Au.Io.print_bool (is_c (Au.Io.In_source.stdin ()) Infer) ;
      [%expect {| false |}]

    let%expect_test "is_c: infer on C-typed stdin" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.stdin ~file_type:"c" ()) Infer) ;
      [%expect {| true |}]

    let%expect_test "is_c: infer on asm-typed stdin" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.stdin ~file_type:"s" ()) Infer) ;
      [%expect {| false |}]

    let%expect_test "is_c: infer on C-typed file" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.file (Fpath.v "hello.c")) Infer) ;
      [%expect {| true |}]

    let%expect_test "is_c: infer on asm-typed file" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.file (Fpath.v "hello.s")) Infer) ;
      [%expect {| false |}]

    let%expect_test "is_c: override C on untyped stdin" =
      Au.Io.print_bool (is_c (Au.Io.In_source.stdin ()) C) ;
      [%expect {| true |}]

    let%expect_test "is_c: override C on C-typed stdin" =
      Au.Io.print_bool (is_c (Au.Io.In_source.stdin ~file_type:"c" ()) C) ;
      [%expect {| true |}]

    let%expect_test "is_c: override C on asm-typed stdin" =
      Au.Io.print_bool (is_c (Au.Io.In_source.stdin ~file_type:"s" ()) C) ;
      [%expect {| true |}]

    let%expect_test "is_c: override C on C-typed file" =
      Au.Io.print_bool (is_c (Au.Io.In_source.file (Fpath.v "hello.c")) C) ;
      [%expect {| true |}]

    let%expect_test "is_c: override C on asm-typed file" =
      Au.Io.print_bool (is_c (Au.Io.In_source.file (Fpath.v "hello.s")) C) ;
      [%expect {| true |}]

    let%expect_test "is_c: override non-C on untyped stdin" =
      Au.Io.print_bool (is_c (Au.Io.In_source.stdin ()) Assembly) ;
      [%expect {| false |}]

    let%expect_test "is_c: override non-C on C-typed stdin" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.stdin ~file_type:"c" ()) Assembly) ;
      [%expect {| false |}]

    let%expect_test "is_c: override non-C on asm-typed stdin" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.stdin ~file_type:"s" ()) Assembly) ;
      [%expect {| false |}]

    let%expect_test "is_c: override non-C on C-typed file" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.file (Fpath.v "hello.c")) Assembly) ;
      [%expect {| false |}]

    let%expect_test "is_c: override non-C on asm-typed file" =
      Au.Io.print_bool
        (is_c (Au.Io.In_source.file (Fpath.v "hello.s")) Assembly) ;
      [%expect {| false |}]
  end )
