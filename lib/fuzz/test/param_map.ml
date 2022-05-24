(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "get_action_cap" =
  ( module struct
    let test (lo : int) (hi : int) (f : Src.Flag.t) : unit =
      let pm =
        Src.Param_map.make
          ~flags:
            (Map.of_alist_exn
               (module Common.Id)
               [(Src.Config_tables.extra_action_flag, f)] )
          ~params:
            (Map.of_alist_exn
               (module Common.Id)
               [ (Src.Config_tables.action_cap_lower_param, lo)
               ; (Src.Config_tables.action_cap_upper_param, hi) ] )
          ()
      in
      let random = Splittable_random.State.of_int 0 in
      let result =
        Or_error.combine_errors
          (List.init 15 ~f:(fun _ -> Src.Param_map.get_action_cap pm ~random))
      in
      Utils.My_format.fdump Stdio.stdout
        Fmt.(result ~ok:(list ~sep:comma int) ~error:Error.pp)
        result

    let%expect_test "bad: hi < lo" =
      test 10 0 (Src.Flag.exact true) ;
      [%expect
        {|
      (("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))
       ("Upper action cap is below lower action cap" (upper 0) (lower 10))) |}]

    let%expect_test "hi = lo, always take" =
      test 10 10 (Src.Flag.exact true) ;
      [%expect
        {| 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 |}]

    let%expect_test "hi = lo, never take" =
      test 10 10 (Src.Flag.exact false) ;
      [%expect
        {| 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 |}]

    let%expect_test "hi = lo, 50/50" =
      test 10 10 (Or_error.ok_exn (Src.Flag.try_make ~wins:1 ~losses:1)) ;
      [%expect
        {| 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 |}]

    let%expect_test "hi > lo, always take" =
      test 10 1000 (Src.Flag.exact true) ;
      [%expect
        {|
      1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
      1000, 1000 |}]

    let%expect_test "hi > lo, never take" =
      test 10 1000 (Src.Flag.exact false) ;
      [%expect
        {| 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 |}]

    let%expect_test "hi > lo, 1:1" =
      test 10 1000 (Or_error.ok_exn (Src.Flag.try_make ~wins:1 ~losses:1)) ;
      [%expect
        {| 11, 11, 10, 13, 12, 13, 13, 10, 10, 10, 11, 15, 11, 10, 11 |}]

    let%expect_test "hi > lo, 5:1" =
      test 10 1000 (Or_error.ok_exn (Src.Flag.try_make ~wins:5 ~losses:1)) ;
      [%expect
        {| 37, 11, 15, 14, 10, 24, 17, 12, 12, 18, 17, 11, 10, 16, 33 |}]

    let%expect_test "hi > lo, 10:1" =
      test 10 1000 (Or_error.ok_exn (Src.Flag.try_make ~wins:10 ~losses:1)) ;
      [%expect
        {| 20, 10, 29, 39, 13, 13, 13, 11, 42, 10, 13, 29, 17, 17, 30 |}]

    let%expect_test "hi > lo, 50:1" =
      test 10 1000 (Or_error.ok_exn (Src.Flag.try_make ~wins:50 ~losses:1)) ;
      [%expect
        {| 29, 25, 212, 12, 15, 54, 16, 73, 13, 48, 16, 58, 23, 50, 220 |}]

    let%expect_test "hi > lo, 100:1" =
      test 10 1000 (Or_error.ok_exn (Src.Flag.try_make ~wins:100 ~losses:1)) ;
      [%expect
        {| 28, 177, 32, 150, 39, 23, 256, 84, 75, 29, 38, 27, 325, 117, 184 |}]
  end )
