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
open Stdio
include Act_utils.Io

let%test_module "filename_no_ext" =
  ( module struct
    let%expect_test "example" =
      printf "%s\n" (filename_no_ext Fpath.(v "foo" / "bar" / "baz.c")) ;
      [%expect {| baz |}]

    let%expect_test "example with double extension" =
      printf "%s\n"
        (filename_no_ext Fpath.(v "foo" / "bar" / "baz.c.litmus")) ;
      [%expect {| baz |}]
  end )

let%test_module "In_source" =
  ( module struct
    open In_source

    let%expect_test "file_type: file with two extensions" =
      Fmt.(pr "%a@." (option string))
        (file_type (file (Fpath.v "iriw.c.litmus"))) ;
      [%expect {| litmus |}]

    let%expect_test "file_type: stdin with specific type" =
      Fmt.(pr "%a@." (option string))
        (file_type (stdin ~file_type:"litmus" ())) ;
      [%expect {| litmus |}]
  end )
