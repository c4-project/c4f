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
open Act_utils.Fs

let%test_module "filter_files" =
  ( module struct
    let test_files = ["stdio.h"; "conio.h"; "main.c"; "main.o"; "README"]

    let%expect_test "filter_files: no filter" =
      let result = filter_files (List.map ~f:Fpath.v test_files) in
      print_s [%sexp (List.map ~f:Fpath.to_string result : string list)] ;
      [%expect {| (stdio.h conio.h main.c main.o README) |}]

    let%expect_test "filter_files: filter" =
      let result = filter_files ~ext:"c" (List.map ~f:Fpath.v test_files) in
      print_s [%sexp (List.map ~f:Fpath.to_string result : string list)] ;
      [%expect {| (main.c) |}]
  end )

let%test_module "subpaths" =
  ( module struct
    let%expect_test "example absolute path" =
      Fmt.(
        pr "@[%a@]@." (list ~sep:sp Fpath.pp)
          (subpaths (Fpath.v "/usr/local/etc/blah/burble/baz"))) ;
      [%expect
        {|
      / /usr/ /usr/local/ /usr/local/etc/ /usr/local/etc/blah/
      /usr/local/etc/blah/burble/ /usr/local/etc/blah/burble/baz |}]

    let%expect_test "example relative path" =
      Fmt.(
        pr "@[%a@]@." (list ~sep:sp Fpath.pp)
          (subpaths (Fpath.v "../inky/pinky/parlez/vous"))) ;
      [%expect
        {| ../ ../inky/ ../inky/pinky/ ../inky/pinky/parlez/ ../inky/pinky/parlez/vous |}]
  end )
