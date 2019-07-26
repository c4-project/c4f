(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
