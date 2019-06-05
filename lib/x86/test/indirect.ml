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
open Act_x86

let%test_module "as_disp_only" =
  ( module struct
    let test (ind : Indirect.t) : unit =
      ind |> Indirect.as_disp_only |> [%sexp_of: Disp.t option] |> print_s

    let%expect_test "positive example (symbolic)" =
      test (Indirect.make ~disp:(Disp.symbolic "foo") ()) ;
      [%expect {| ((Symbolic foo)) |}]

    let%expect_test "positive example (numeric)" =
      test (Indirect.make ~disp:(Disp.numeric 42) ()) ;
      [%expect {| ((Numeric 42)) |}]

    let%expect_test "negative example (no displacement)" =
      test (Indirect.make ~base:`EAX ()) ;
      [%expect {| () |}]

    let%expect_test "negative example (other fields)" =
      test (Indirect.make ~base:`EAX ~disp:(Disp.symbolic "foo") ()) ;
      [%expect {| () |}]
  end )

let%test_module "as_symbolic_disp_only" =
  ( module struct
    let test (ind : Indirect.t) : unit =
      ind |> Indirect.as_symbolic_disp_only |> Option.iter ~f:print_endline

    let%expect_test "positive example" =
      test (Indirect.make ~disp:(Disp.symbolic "foo") ()) ;
      [%expect {| foo |}]

    let%expect_test "negative example (numeric)" =
      test (Indirect.make ~disp:(Disp.numeric 42) ()) ;
      [%expect {| |}]

    let%expect_test "negative example (no displacement)" =
      test (Indirect.make ~base:`EAX ()) ;
      [%expect {| |}]

    let%expect_test "negative example (other fields)" =
      test (Indirect.make ~base:`EAX ~disp:(Disp.symbolic "foo") ()) ;
      [%expect {| |}]
  end )
