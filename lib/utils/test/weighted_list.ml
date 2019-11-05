(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Core_kernel
open Act_utils.Weighted_list

let%test_module "sampling and conversion tests" =
  ( module struct
    let wl_one = from_alist_exn [("kappa", 1)]

    let wl = from_alist_exn [("keepo", 2); ("frankerz", 5); ("kappa", 1)]

    let%test_unit "sample: sampling from list of one weight-1 item returns \
                   that item" =
      Quickcheck.test (sample_gen_exn wl_one) ~sexp_of:[%sexp_of: string]
        ~f:
          ([%test_result: string] ~here:[[%here]] ~equal:[%equal: string]
             ~expect:"kappa")

    let%test_unit "sample: sampling can return the last item" =
      Quickcheck.test_can_generate (sample_gen_exn wl)
        ~sexp_of:[%sexp_of: string]
        ~f:([%equal: string] "kappa")

    let%expect_test "init: example run" =
      let f = Fmt.pr "@[%s -> %i@]@." in
      iter ~f wl ;
      [%expect {|
      keepo -> 2
      frankerz -> 5
      kappa -> 1 |}]
  end )
