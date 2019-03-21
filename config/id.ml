(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core

module T = struct
  (** [t] is the type of compiler IDs. *)
  type t = String.Caseless.t list [@@deriving compare, hash, sexp, bin_io]

  let allowed_id_splits = [ '.'; ' '; '/'; '\\' ]
  let of_string = String.split_on_chars ~on:allowed_id_splits
  let to_string = String.concat ~sep:"."
  let module_name = "act.Lib.Id"
end

include T
include Identifiable.Make (T)

let%expect_test "equality is case-insensitive" =
  Sexp.output_hum
    Out_channel.stdout
    [%sexp
      (Ordering.of_int (compare (of_string "foo.bar.baz") (of_string "Foo.Bar.BAZ"))
        : Ordering.t)];
  [%expect {| Equal |}]
;;

let%expect_test "parse Foo.Bar.BAZ (note case) as a spec ID" =
  Sexp.output_hum Out_channel.stdout [%sexp (of_string "Foo.Bar.BAZ" : t)];
  [%expect {| (Foo Bar BAZ) |}]
;;

let%expect_test "parse foo.bar.baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout [%sexp (of_string "foo.bar.baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse foo/bar/baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout [%sexp (of_string "foo/bar/baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse foo\\bar\\baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout [%sexp (of_string "foo\\bar\\baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse 'foo bar baz' as a spec ID" =
  Sexp.output_hum Out_channel.stdout [%sexp (of_string "foo bar baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let to_string_list : t -> string list = Fn.id

let is_prefix id ~prefix =
  List.is_prefix
    (to_string_list id)
    ~prefix:(to_string_list prefix)
    ~equal:String.Caseless.equal
;;

let%expect_test "is_prefix: valid prefix, identical" =
  Stdio.print_s
    [%sexp (is_prefix (of_string "foo.bar") ~prefix:(of_string "foo.bar") : bool)];
  [%expect {| true |}]
;;

let%expect_test "is_prefix: valid prefix, identical modulo case" =
  Stdio.print_s
    [%sexp (is_prefix (of_string "foo.bar") ~prefix:(of_string "Foo.Bar") : bool)];
  [%expect {| true |}]
;;

let%expect_test "is_prefix: valid prefix, same case" =
  Sexp.output_hum
    Out_channel.stdout
    [%sexp (is_prefix (of_string "foo.bar.baz") ~prefix:(of_string "foo.bar") : bool)];
  [%expect {| true |}]
;;

let%expect_test "is_prefix: valid prefix, different case" =
  Stdio.print_s
    [%sexp (is_prefix (of_string "foo.BAR.baz") ~prefix:(of_string "FOO.bar") : bool)];
  [%expect {| true |}]
;;

let%expect_test "is_prefix: invalid prefix (but valid string prefix)" =
  Stdio.print_s
    [%sexp (is_prefix (of_string "foo.BAR.baz") ~prefix:(of_string "foo.BA") : bool)];
  [%expect {| false |}]
;;

let%expect_test "is_prefix: invalid prefix" =
  Stdio.print_s
    [%sexp (is_prefix (of_string "foo.bar") ~prefix:(of_string "foo.bar.baz") : bool)];
  [%expect {| false |}]
;;

let has_tag id element = List.mem id element ~equal:String.Caseless.equal

let%expect_test "has_tag: valid, same case" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "foo" : bool)];
  [%expect {| true |}]
;;

let%expect_test "has_tag: valid, different case" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "FOO" : bool)];
  [%expect {| true |}]
;;

let%expect_test "has_tag: invalid, but is a (oversized) substring" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "foo." : bool)];
  [%expect {| false |}]
;;

let%expect_test "has_tag: invalid, but is a (undersized) substring" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "fo" : bool)];
  [%expect {| false |}]
;;

let%expect_test "has_tag: invalid, multiple tags" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "foo.bar" : bool)];
  [%expect {| false |}]
;;

let%expect_test "has_tag: invalid, empty string" =
  Stdio.print_s
    [%sexp (has_tag (of_string "foo.BAR.baz") "" : bool)];
  [%expect {| false |}]
;;

module Property = struct
  type id = t

  (* Putting this in a sub-module so as not to shadow [contains] when
     we define [eval]. *)
  module M = struct
    type t =
      | Has_tag of string
      | Has_prefix of string
      | Is of string
    [@@deriving sexp, variants]
  end

  let eval id = function
    | M.Has_tag s -> has_tag id s
    | M.Has_prefix s -> is_prefix ~prefix:(of_string s) id
    | M.Is s -> equal (of_string s) id
  ;;

  include M

  let tree_docs : Property.Tree_doc.t =
    [ ("has_tag"
      , { args = [ "STRING" ]
        ; details =
            {| Requires that any of the dot-separated items in this ID
             matches the argument. |}
        })
    ; ("has_prefix"
      , { args = [ "PARTIAL-ID" ]
        ; details = {| Requires that the ID starts with the argument. |}
        })
    ; ("is"
      , { args = [ "ID" ]
        ; details = {| Requires that the ID directly matches the argument. |}
        })
    ]
  ;;

  let pp_tree : unit Fmt.t =
    Property.Tree_doc.pp tree_docs (List.map ~f:fst Variants.descriptions)
  ;;

  let%expect_test "all properties have documentation" =
    let num_passes =
      Variants.descriptions
      |> List.map ~f:fst
      |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
      |> List.count ~f:not
    in
    Fmt.pr "@[<v>%d@]@." num_passes;
    [%expect {| 0 |}]
  ;;

  let eval_b id expr = Blang.eval expr (eval id)

  let%expect_test "eval_b: sample passing expression" =
    let query =
      Blang.t_of_sexp t_of_sexp (Sexp.of_string "(and (has_tag bar) (has_prefix foo))")
    in
    let id = of_string "Foo.Bar.Baz" in
    Sexp.output_hum Out_channel.stdout [%sexp (eval_b id query : bool)];
    [%expect {| true |}]
  ;;

  let%expect_test "eval_b: sample failing expression" =
    let query =
      Blang.t_of_sexp t_of_sexp (Sexp.of_string "(and (has_tag bar) (is foo.bar))")
    in
    let id = of_string "Foo.Bar.Baz" in
    Sexp.output_hum Out_channel.stdout [%sexp (eval_b id query : bool)];
    [%expect {| false |}]
  ;;
end
