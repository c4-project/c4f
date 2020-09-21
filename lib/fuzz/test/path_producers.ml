(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import
open Base_quickcheck

let print_sample (generator : Src.Path.Flagged.t Generator.t) : unit =
  Act_utils.My_quickcheck.print_sample
    ~printer:(Fmt.pr "@[%a@]@." Src.Path.Flagged.pp)
    ( module struct
      type t = Src.Path.Flagged.t [@@deriving compare, sexp]

      let quickcheck_generator = generator

      let quickcheck_observer = Observer.opaque

      let quickcheck_shrinker = Shrinker.atomic
    end )

let%test_module "sample path output on example code" =
  ( module struct
    let test (kind : Src.Path_kind.t) (filter : Src.Path_filter.t) : unit =
      let test = Lazy.force Subject.Test_data.test in
      let paths = Src.Path_producers.try_gen_with_flags test ~filter ~kind in
      print_sample (Or_error.ok_exn paths)

    let%expect_test "insert with no filtering" =
      test Insert Src.Path_filter.zero ;
      [%expect
        {|
              P0!Stms!Insert[0] {}
              P0!Stms!Insert[2] {}
              P0!Stms!Insert[3] {}
              P0!Stms!Insert[6] {}
              P0!Stms!Stm[3]!If!True!Insert[0] {}
              P0!Stms!Stm[3]!If!True!Insert[1] {}
              P0!Stms!Stm[3]!If!True!Insert[2] {}
              P0!Stms!Stm[4]!If!False!Insert[0] {}
              P0!Stms!Stm[5]!Flow-block!Body!Insert[0] {in-execute-multi, in-loop}
              P0!Stms!Stm[6]!Flow-block!Body!Insert[0] {in-execute-multi, in-loop}
              P0!Stms!Stm[6]!Flow-block!Body!Insert[1] {in-execute-multi, in-loop}
              P0!Stms!Stm[7]!Flow-block!Body!Insert[1] {in-dead-code, in-loop}
              P1!Stms!Insert[1] {}
              P1!Stms!Stm[1]!If!True!Insert[0] {} |}]

    let%expect_test "insert with top anchoring" =
      test Insert (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        P0!Stms!Insert[0] {}
        P0!Stms!Stm[3]!If!False!Insert[0] {in-dead-code}
        P0!Stms!Stm[3]!If!True!Insert[0] {}
        P0!Stms!Stm[4]!If!False!Insert[0] {}
        P0!Stms!Stm[6]!Flow-block!Body!Insert[0] {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Insert[0] {in-dead-code, in-loop}
        P1!Stms!Insert[0] {}
        P1!Stms!Stm[1]!If!False!Insert[0] {in-dead-code}
        P1!Stms!Stm[1]!If!True!Insert[0] {} |}]

    let%expect_test "insert with bottom anchoring" =
      test Insert (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        P0!Stms!Insert[8] {}
        P0!Stms!Stm[3]!If!False!Insert[0] {in-dead-code}
        P0!Stms!Stm[3]!If!True!Insert[2] {}
        P0!Stms!Stm[4]!If!False!Insert[0] {}
        P0!Stms!Stm[6]!Flow-block!Body!Insert[1] {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Insert[1] {in-dead-code, in-loop}
        P1!Stms!Insert[2] {}
        P1!Stms!Stm[1]!If!False!Insert[1] {in-dead-code}
        P1!Stms!Stm[1]!If!True!Insert[0] {} |}]

    let%expect_test "insert with full anchoring" =
      test Insert (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        P0!Stms!Stm[3]!If!False!Insert[0] {in-dead-code}
        P0!Stms!Stm[4]!If!False!Insert[0] {}
        P1!Stms!Stm[1]!If!True!Insert[0] {} |}]

    let%expect_test "insert with execute-multi filtering" =
      test Insert Src.Path_filter.(forbid_flag In_execute_multi) ;
      [%expect
        {|
              P0!Stms!Insert[0] {}
              P0!Stms!Insert[3] {}
              P0!Stms!Insert[4] {}
              P0!Stms!Stm[3]!If!False!Insert[0] {in-dead-code}
              P0!Stms!Stm[3]!If!True!Insert[0] {}
              P0!Stms!Stm[3]!If!True!Insert[1] {}
              P0!Stms!Stm[3]!If!True!Insert[2] {}
              P0!Stms!Stm[4]!If!False!Insert[0] {}
              P0!Stms!Stm[4]!If!True!Insert[0] {in-dead-code}
              P0!Stms!Stm[4]!If!True!Insert[1] {in-dead-code}
              P0!Stms!Stm[7]!Flow-block!Body!Insert[1] {in-dead-code, in-loop}
              P1!Stms!Insert[2] {}
              P1!Stms!Stm[1]!If!False!Insert[1] {in-dead-code} |}]

    let%expect_test "insert with thread filtering" =
      test Insert
        Src.Path_filter.(in_threads_only (Set.singleton (module Int) 1)) ;
      [%expect
        {|
              P1!Stms!Insert[0] {}
              P1!Stms!Insert[1] {}
              P1!Stms!Insert[2] {}
              P1!Stms!Stm[1]!If!False!Insert[0] {in-dead-code}
              P1!Stms!Stm[1]!If!False!Insert[1] {in-dead-code}
              P1!Stms!Stm[1]!If!True!Insert[0] {} |}]

    let%expect_test "insert with dead-code filtering" =
      test Insert Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
            P0!Stms!Stm[3]!If!False!Insert[0] {in-dead-code}
            P0!Stms!Stm[4]!If!True!Insert[0] {in-dead-code}
            P0!Stms!Stm[4]!If!True!Insert[1] {in-dead-code}
            P0!Stms!Stm[7]!Flow-block!Body!Insert[0] {in-dead-code, in-loop}
            P0!Stms!Stm[7]!Flow-block!Body!Insert[1] {in-dead-code, in-loop}
            P1!Stms!Stm[1]!If!False!Insert[1] {in-dead-code} |}]

    let%expect_test "transform with no filtering" =
      test Transform Src.Path_filter.zero ;
      [%expect
        {|
            P0!Stms!Stm[0]!This {}
            P0!Stms!Stm[1]!This {}
            P0!Stms!Stm[2]!This {}
            P0!Stms!Stm[3]!If!True!Stm[1]!This {}
            P0!Stms!Stm[3]!This {}
            P0!Stms!Stm[4]!This {}
            P0!Stms!Stm[6]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
            P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop}
            P0!Stms!Stm[7]!This {}
            P1!Stms!Stm[0]!This {}
            P1!Stms!Stm[1]!This {} |}]

    let%expect_test "transform with top anchoring" =
      test Transform (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        P0!Stms!Stm[0]!This {}
        P0!Stms!Stm[3]!If!True!Stm[0]!This {}
        P0!Stms!Stm[4]!If!True!Stm[0]!This {in-dead-code}
        P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop}
        P1!Stms!Stm[0]!This {}
        P1!Stms!Stm[1]!If!False!Stm[0]!This {in-dead-code} |}]

    let%expect_test "transform with bottom anchoring" =
      test Transform (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        P0!Stms!Stm[3]!If!True!Stm[1]!This {}
        P0!Stms!Stm[4]!If!True!Stm[0]!This {in-dead-code}
        P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop}
        P0!Stms!Stm[7]!This {}
        P1!Stms!Stm[1]!If!False!Stm[0]!This {in-dead-code}
        P1!Stms!Stm[1]!This {} |}]

    let%expect_test "transform with full anchoring" =
      test Transform (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        P0!Stms!Stm[4]!If!True!Stm[0]!This {in-dead-code}
        P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop}
        P1!Stms!Stm[1]!If!False!Stm[0]!This {in-dead-code} |}]

    let%expect_test "transform with filtering to if statements" =
      test Transform
        Src.Path_filter.(require_end_check (Stm_class (Is, [If]))) ;
      [%expect
        {|
            P0!Stms!Stm[3]!This {}
            P0!Stms!Stm[4]!This {}
            P1!Stms!Stm[1]!This {} |}]

    let%expect_test "transform with filtering to dead code" =
      test Transform Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
              P0!Stms!Stm[4]!If!True!Stm[0]!This {in-dead-code}
              P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop}
              P1!Stms!Stm[1]!If!False!Stm[0]!This {in-dead-code} |}]

    let%expect_test "transform with filtering to loops" =
      test Transform Src.Path_filter.(require_flag In_loop) ;
      [%expect
        {|
        P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Stm[0]!This {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Stm[0]!This {in-dead-code, in-loop} |}]

    let%expect_test "transform-list" =
      test Transform_list Src.Path_filter.zero ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!True!Range[0, 1] {}
              P0!Stms!Stm[3]!If!True!Range[1, 1] {}
              P0!Stms!Stm[4]!If!True!Range[1, 0] {in-dead-code}
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0] {in-execute-multi, in-loop}
              P0!Stms!Range[0, 5] {}
              P0!Stms!Range[0, 6] {}
              P0!Stms!Range[1, 0] {}
              P0!Stms!Range[1, 3] {}
              P0!Stms!Range[4, 1] {}
              P0!Stms!Range[4, 2] {}
              P0!Stms!Range[4, 3] {}
              P0!Stms!Range[4, 4] {}
              P0!Stms!Range[5, 1] {}
              P0!Stms!Range[5, 2] {}
              P0!Stms!Range[5, 3] {}
              P0!Stms!Range[6, 1] {}
              P0!Stms!Range[6, 2] {}
              P0!Stms!Range[7, 0] {}
              P1!Stms!Range[1, 1] {}
              P1!Stms!Range[2, 0] {} |}]

    let%expect_test "transform-list with top anchoring" =
      test Transform_list (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        P0!Stms!Stm[3]!If!True!Range[0, 0] {}
        P0!Stms!Stm[3]!If!True!Range[0, 1] {}
        P0!Stms!Stm[3]!If!True!Range[0, 2] {}
        P0!Stms!Stm[4]!If!False!Range[0, 0] {}
        P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0] {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Range[0, 0] {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Range[0, 1] {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Range[0, 1] {in-dead-code, in-loop}
        P0!Stms!Range[0, 0] {}
        P0!Stms!Range[0, 2] {}
        P0!Stms!Range[0, 3] {}
        P0!Stms!Range[0, 6] {}
        P1!Stms!Stm[1]!If!True!Range[0, 0] {}
        P1!Stms!Range[0, 1] {} |}]

    let%expect_test "transform-list with bottom anchoring" =
      test Transform_list (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        P0!Stms!Stm[3]!If!True!Range[0, 2] {}
        P0!Stms!Stm[3]!If!True!Range[1, 1] {}
        P0!Stms!Stm[3]!If!True!Range[2, 0] {}
        P0!Stms!Stm[4]!If!False!Range[0, 0] {}
        P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0] {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Range[0, 1] {in-execute-multi, in-loop}
        P0!Stms!Stm[6]!Flow-block!Body!Range[1, 0] {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Range[0, 1] {in-dead-code, in-loop}
        P0!Stms!Range[2, 6] {}
        P0!Stms!Range[5, 3] {}
        P0!Stms!Range[6, 2] {}
        P0!Stms!Range[8, 0] {}
        P1!Stms!Stm[1]!If!True!Range[0, 0] {}
        P1!Stms!Range[1, 1] {} |}]

    let%expect_test "transform-list with full anchoring" =
      test Transform_list (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        P0!Stms!Stm[3]!If!False!Range[0, 0] {in-dead-code}
        P0!Stms!Stm[3]!If!True!Range[0, 2] {}
        P0!Stms!Stm[4]!If!False!Range[0, 0] {}
        P0!Stms!Stm[6]!Flow-block!Body!Range[0, 1] {in-execute-multi, in-loop}
        P0!Stms!Stm[7]!Flow-block!Body!Range[0, 1] {in-dead-code, in-loop}
        P0!Stms!Range[0, 8] {}
        P1!Stms!Stm[1]!If!False!Range[0, 1] {in-dead-code}
        P1!Stms!Stm[1]!If!True!Range[0, 0] {}
        P1!Stms!Range[0, 2] {} |}]

    let%expect_test "transform-list with filtering to dead code" =
      test Transform_list Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0] {in-dead-code}
              P0!Stms!Stm[4]!If!True!Range[0, 0] {in-dead-code}
              P0!Stms!Stm[4]!If!True!Range[0, 1] {in-dead-code}
              P0!Stms!Stm[4]!If!True!Range[1, 0] {in-dead-code}
              P0!Stms!Stm[7]!Flow-block!Body!Range[0, 0] {in-dead-code, in-loop}
              P0!Stms!Stm[7]!Flow-block!Body!Range[0, 1] {in-dead-code, in-loop}
              P0!Stms!Stm[7]!Flow-block!Body!Range[1, 0] {in-dead-code, in-loop}
              P1!Stms!Stm[1]!If!False!Range[0, 0] {in-dead-code}
              P1!Stms!Stm[1]!If!False!Range[0, 1] {in-dead-code}
              P1!Stms!Stm[1]!If!False!Range[1, 0] {in-dead-code} |}]

    let%expect_test "transform-list with filtering to if statements" =
      test Transform_list
        Src.Path_filter.(require_end_check (Stm_class (Is, [If]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!True!Range[1, 0] {}
              P0!Stms!Stm[4]!If!False!Range[0, 0] {}
              P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0] {in-execute-multi, in-loop}
              P0!Stms!Stm[6]!Flow-block!Body!Range[1, 0] {in-execute-multi, in-loop}
              P0!Stms!Stm[7]!Flow-block!Body!Range[0, 0] {in-dead-code, in-loop}
              P0!Stms!Range[2, 0] {}
              P0!Stms!Range[3, 0] {}
              P0!Stms!Range[3, 1] {}
              P0!Stms!Range[4, 0] {}
              P0!Stms!Range[6, 0] {}
              P0!Stms!Range[7, 0] {}
              P0!Stms!Range[8, 0] {}
              P1!Stms!Stm[1]!If!False!Range[0, 0] {in-dead-code}
              P1!Stms!Stm[1]!If!True!Range[0, 0] {}
              P1!Stms!Range[1, 1] {} |}]

    let%expect_test "transform-list with filtering to non-labels" =
      (* TODO(@MattWindsor91): should this be excluding [0, 0]? *)
      test Transform_list
        Src.Path_filter.(
          require_end_check (Stm_class (Is_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0] {in-dead-code}
              P0!Stms!Stm[3]!If!True!Range[0, 0] {}
              P0!Stms!Stm[3]!If!True!Range[2, 0] {}
              P0!Stms!Stm[4]!If!True!Range[0, 1] {in-dead-code}
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 1] {in-execute-multi, in-loop}
              P0!Stms!Range[0, 1] {}
              P0!Stms!Range[1, 7] {}
              P0!Stms!Range[2, 0] {}
              P0!Stms!Range[2, 4] {}
              P0!Stms!Range[3, 0] {}
              P0!Stms!Range[3, 1] {}
              P0!Stms!Range[5, 1] {}
              P0!Stms!Range[5, 3] {}
              P0!Stms!Range[7, 0] {}
              P1!Stms!Stm[1]!If!False!Range[0, 0] {in-dead-code}
              P1!Stms!Range[0, 0] {}
              P1!Stms!Range[2, 0] {} |}]

    let%expect_test "transform-list with filtering to recursive non-labels" =
      test Transform_list
        Src.Path_filter.(
          require_end_check (Stm_class (Has_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0] {in-dead-code}
              P0!Stms!Stm[3]!If!True!Range[2, 0] {}
              P0!Stms!Stm[4]!If!True!Range[1, 0] {in-dead-code}
              P0!Stms!Stm[6]!Flow-block!Body!Range[0, 0] {in-execute-multi, in-loop}
              P0!Stms!Stm[6]!Flow-block!Body!Range[1, 0] {in-execute-multi, in-loop}
              P0!Stms!Stm[7]!Flow-block!Body!Range[0, 0] {in-dead-code, in-loop}
              P0!Stms!Range[0, 0] {}
              P0!Stms!Range[0, 1] {}
              P0!Stms!Range[1, 0] {}
              P0!Stms!Range[1, 1] {}
              P0!Stms!Range[2, 0] {}
              P0!Stms!Range[3, 0] {}
              P0!Stms!Range[4, 1] {}
              P0!Stms!Range[5, 0] {}
              P0!Stms!Range[5, 2] {}
              P0!Stms!Range[7, 0] {}
              P1!Stms!Stm[1]!If!True!Range[0, 0] {}
              P1!Stms!Range[1, 1] {} |}]
  end )
