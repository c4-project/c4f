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

(** [Spec] contains general interfaces for dealing with specifications
    of machines and compilers. *)

open Core

module T = struct
  (** [t] is the type of compiler IDs. *)
  type t = string list [@@deriving compare, hash, sexp, bin_io]

  let allowed_id_splits = [ '.' ; ' '; '/'; '\\']

  let of_string = String.split_on_chars ~on:allowed_id_splits

  let to_string = String.concat ~sep:"."

  let module_name = "act.Lib.Id"
end

include T
include Identifiable.Make (T)

let to_string_list = Fn.id

let%expect_test "parse foo.bar.baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (of_string "foo.bar.baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse foo/bar/baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (of_string "foo/bar/baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse foo\\bar\\baz as a spec ID" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (of_string "foo\\bar\\baz" : t)];
  [%expect {| (foo bar baz) |}]
;;

let%expect_test "parse 'foo bar baz' as a spec ID" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (of_string "foo bar baz" : t)];
  [%expect {| (foo bar baz) |}]
;;
