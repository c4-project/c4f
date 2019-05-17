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
open Act_common.Id

let%expect_test "equality is case-insensitive" =
  print_s
    [%sexp
      ( Ordering.of_int
          (compare (of_string "foo.bar.baz") (of_string "Foo.Bar.BAZ"))
        : Ordering.t )] ;
  [%expect {| Equal |}]

let%test_module "of_string" =
  ( module struct
    let%expect_test "parse Foo.Bar.BAZ (note case) as a spec ID" =
      print_s [%sexp (of_string "Foo.Bar.BAZ" : t)] ;
      [%expect {| (Foo Bar BAZ) |}]

    let%expect_test "parse foo.bar.baz as a spec ID" =
      print_s [%sexp (of_string "foo.bar.baz" : t)] ;
      [%expect {| (foo bar baz) |}]

    let%expect_test "parse foo/bar/baz as a spec ID" =
      print_s [%sexp (of_string "foo/bar/baz" : t)] ;
      [%expect {| (foo bar baz) |}]

    let%expect_test "parse foo\\bar\\baz as a spec ID" =
      print_s [%sexp (of_string "foo\\bar\\baz" : t)] ;
      [%expect {| (foo bar baz) |}]

    let%expect_test "parse 'foo bar baz' as a spec ID" =
      print_s [%sexp (of_string "foo bar baz" : t)] ;
      [%expect {| (foo bar baz) |}]
  end )

let%test_module "is_prefix" =
  ( module struct
    let%expect_test "valid prefix, identical" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.bar") ~prefix:(of_string "foo.bar")
            : bool )] ;
      [%expect {| true |}]

    let%expect_test "valid prefix, identical modulo case" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.bar") ~prefix:(of_string "Foo.Bar")
            : bool )] ;
      [%expect {| true |}]

    let%expect_test "valid prefix, same case" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.bar.baz") ~prefix:(of_string "foo.bar")
            : bool )] ;
      [%expect {| true |}]

    let%expect_test "valid prefix, different case" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.BAR.baz") ~prefix:(of_string "FOO.bar")
            : bool )] ;
      [%expect {| true |}]

    let%expect_test "invalid prefix (but valid string prefix)" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.BAR.baz") ~prefix:(of_string "foo.BA")
            : bool )] ;
      [%expect {| false |}]

    let%expect_test "invalid prefix" =
      print_s
        [%sexp
          ( is_prefix (of_string "foo.bar") ~prefix:(of_string "foo.bar.baz")
            : bool )] ;
      [%expect {| false |}]
  end )

let%test_module "has_tag" =
  ( module struct
    let%expect_test "valid, same case" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "foo" : bool)] ;
      [%expect {| true |}]

    let%expect_test "valid, different case" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "FOO" : bool)] ;
      [%expect {| true |}]

    let%expect_test "invalid, but is a (oversized) substring" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "foo." : bool)] ;
      [%expect {| false |}]

    let%expect_test "invalid, but is a (undersized) substring" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "fo" : bool)] ;
      [%expect {| false |}]

    let%expect_test "invalid, multiple tags" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "foo.bar" : bool)] ;
      [%expect {| false |}]

    let%expect_test "invalid, empty string" =
      print_s [%sexp (has_tag (of_string "foo.BAR.baz") "" : bool)] ;
      [%expect {| false |}]
  end )

let%test_module "Property" =
  ( module struct
    open Property
    open Core_kernel

    let%expect_test "eval_b: sample passing expression" =
      let query =
        Blang.t_of_sexp t_of_sexp
          (Sexp.of_string "(and (has_tag bar) (has_prefix foo))")
      in
      let id = of_string "Foo.Bar.Baz" in
      print_s [%sexp (eval_b id query : bool)] ;
      [%expect {| true |}]

    let%expect_test "eval_b: sample failing expression" =
      let query =
        Blang.t_of_sexp t_of_sexp
          (Sexp.of_string "(and (has_tag bar) (is foo.bar))")
      in
      let id = of_string "Foo.Bar.Baz" in
      print_s [%sexp (eval_b id query : bool)] ;
      [%expect {| false |}]
  end )
