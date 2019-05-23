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

open Stdio
open Act_x86

let%test_module "Abstract classification" =
  ( module struct
    let test (r : Reg.t) : unit =
      r |> Reg.abs_kind |> Act_abstract.Register.Kind.to_string
      |> print_endline

    let%expect_test "EBP is a stack pointer" =
      test `EBP ;
      [%expect {| |}]

    let%expect_test "ESP is a stack pointer" =
      test `ESP ;
      [%expect {| |}]

    let%expect_test "ZF is a special-purpose register" =
      test `ZF ;
      [%expect {| |}]

    let%expect_test "EAX is a general-purpose register" =
      test `EAX ;
      [%expect {| |}]
  end )
