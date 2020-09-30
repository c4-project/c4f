(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_utils.Weighted_list

let%test_module "sampling and conversion tests" =
  ( module struct
    let wl_one = Or_error.ok_exn (from_alist [("kappa", 1)])

    let wl =
      Or_error.ok_exn
        (from_alist [("keepo", 2); ("frankerz", 5); ("kappa", 1)])

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
