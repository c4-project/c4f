(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "gen_int32" =
  ( module struct
    module Gen = struct
      include Src.Constant

      let quickcheck_generator = Src.Constant.gen_int32
    end

    let%expect_test "sample" =
      Utils.My_quickcheck.print_sample ~test_count:30
        ~printer:(Fmt.pr "@[%a@]@." Src.Constant.pp)
        (module Gen) ;
      [%expect
        {|
           -2147483648
           -879720314
           -780780327
           -52859389
           -50348097
           -37287526
           -23556581
           -15464318
           -4713
           -511
           -209
           -18
           -1
           0
           1
           664
           1136
           7471
           7627
           13418
           489744
           10703535
           268435456
           2147483647 |}]

    let%test_unit "generator doesn't panic" =
      Q.Test.run_exn (module Gen) ~f:(fun _ -> ())
  end )

let%test_module "convert" =
  ( module struct
    let test (x : Src.Constant.t) (to_ : Src.Type.Prim.t) : unit =
      Stdio.print_s
        [%sexp (Src.Constant.convert x ~to_ : Src.Constant.t Or_error.t)]

    let%expect_test "truthy int to bool" =
      test (Src.Constant.int 27) Bool ;
      [%expect {| (Ok (Bool true)) |}]

    let%expect_test "falsy int to bool" =
      test (Src.Constant.int 0) Bool ;
      [%expect {| (Ok (Bool false)) |}]

    let%expect_test "truth to int" =
      test Src.Constant.truth Int ;
      [%expect {| (Ok (Int 1)) |}]

    let%expect_test "falsehood to int" =
      test Src.Constant.falsehood Int ;
      [%expect {| (Ok (Int 0)) |}]
  end )
