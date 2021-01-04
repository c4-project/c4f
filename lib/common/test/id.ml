(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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
