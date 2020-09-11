(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

let%test_module "sample path output on example code" =
  ( module struct
    let print_sample (generator : Act_fuzz.Path.Flagged.t Generator.t) : unit
        =
      Act_utils.My_quickcheck.print_sample
        ~printer:(Fmt.pr "@[%a@]@." Act_fuzz.Path.Flagged.pp)
        ( module struct
          type t = Act_fuzz.Path.Flagged.t [@@deriving compare, sexp]

          let quickcheck_generator = generator

          let quickcheck_observer = Observer.opaque

          let quickcheck_shrinker = Shrinker.atomic
        end )

    let test (kind : Act_fuzz.Path_kind.t) (filter : Act_fuzz.Path_filter.t)
        : unit =
      let test = Lazy.force Subject.Test_data.test in
      let paths =
        Act_fuzz.Path_producers.try_gen_with_flags test ~filter ~kind
      in
      print_sample (Or_error.ok_exn paths)

    let%expect_test "try_gen_insert_stm with no filtering" =
      test Insert Act_fuzz.Path_filter.empty ;
      [%expect
        {|
              P0!Stms!Insert[0] []
              P0!Stms!Insert[1] []
              P0!Stms!Insert[4] []
              P0!Stms!Insert[6] []
              P0!Stms!Stm[3]!If!False!Insert[0] [in-dead-code]
              P0!Stms!Stm[3]!If!True!Insert[0] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Insert[1] [in-execute-multi]
              P0!Stms!Stm[4]!If!False!Insert[0] [in-execute-multi]
              P0!Stms!Stm[4]!If!True!Insert[1] [in-dead-code]
              P0!Stms!Stm[5]!Flow-block!Body!Insert[0] [in-dead-code, in-loop]
              P0!Stms!Stm[5]!Flow-block!Body!Insert[1] [in-dead-code, in-loop]
              P1!Stms!Insert[1] []
              P1!Stms!Stm[1]!If!True!Insert[0] [in-execute-multi] |}]

    let%expect_test "try_gen_insert_stm with thread filtering" =
      test Insert
        Act_fuzz.Path_filter.(
          empty |> in_threads_only ~threads:(Set.singleton (module Int) 1)) ;
      [%expect
        {|
              P1!Stms!Insert[0] []
              P1!Stms!Insert[1] []
              P1!Stms!Insert[2] []
              P1!Stms!Stm[1]!If!False!Insert[0] [in-dead-code]
              P1!Stms!Stm[1]!If!False!Insert[1] [in-dead-code]
              P1!Stms!Stm[1]!If!True!Insert[0] [in-execute-multi] |}]

    let%expect_test "try_gen_insert_stm with dead-code filtering" =
      test Insert Act_fuzz.Path_filter.(empty |> in_dead_code_only) ;
      [%expect
        {|
            P0!Stms!Stm[3]!If!False!Insert[0] [in-dead-code]
            P0!Stms!Stm[4]!If!True!Insert[0] [in-dead-code]
            P0!Stms!Stm[4]!If!True!Insert[1] [in-dead-code]
            P0!Stms!Stm[5]!Flow-block!Body!Insert[0] [in-dead-code, in-loop]
            P0!Stms!Stm[5]!Flow-block!Body!Insert[1] [in-dead-code, in-loop]
            P1!Stms!Stm[1]!If!False!Insert[1] [in-dead-code] |}]

    let%expect_test "try_gen_transform_stm with no filtering" =
      test Transform Act_fuzz.Path_filter.empty ;
      [%expect
        {|
            P0!Stms!Stm[0]!This [in-execute-multi]
            P0!Stms!Stm[1]!This [in-execute-multi]
            P0!Stms!Stm[2]!This [in-execute-multi]
            P0!Stms!Stm[3]!This []
            P0!Stms!Stm[4]!If!True!Stm[0]!This [in-dead-code, in-execute-multi]
            P0!Stms!Stm[4]!This []
            P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This
            [in-dead-code, in-execute-multi, in-loop]
            P0!Stms!Stm[5]!This []
            P1!Stms!Stm[0]!This [in-execute-multi]
            P1!Stms!Stm[1]!If!False!Stm[0]!This [in-dead-code, in-execute-multi]
            P1!Stms!Stm[1]!This [] |}]

    let%expect_test "try_gen_transform_stm with filtering to if statements" =
      test Transform
        Act_fuzz.Path_filter.(
          empty |> require_end_check ~check:(Stm_class (Is, [If]))) ;
      [%expect
        {|
            P0!Stms!Stm[3]!This []
            P0!Stms!Stm[4]!This []
            P1!Stms!Stm[1]!This [] |}]

    let%expect_test "try_gen_transform_stm with filtering to dead code" =
      test Transform Act_fuzz.Path_filter.(empty |> in_dead_code_only) ;
      [%expect
        {|
              P0!Stms!Stm[4]!If!True!Stm[0]!This [in-dead-code, in-execute-multi]
              P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This
              [in-dead-code, in-execute-multi, in-loop]
              P1!Stms!Stm[1]!If!False!Stm[0]!This [in-dead-code, in-execute-multi] |}]

    let%expect_test "try_gen_transform_stm with filtering to loops" =
      test Transform Act_fuzz.Path_filter.(empty |> in_loop_only) ;
      [%expect
        {|
        P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This
        [in-dead-code, in-execute-multi, in-loop] |}]

    let%expect_test "try_gen_transform_stm_list" =
      test Transform_list Act_fuzz.Path_filter.empty ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!True!Range[0, 1] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[1, 0] [in-execute-multi]
              P0!Stms!Stm[4]!If!True!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0] [in-dead-code, in-loop]
              P0!Stms!Range[0, 0] []
              P0!Stms!Range[0, 2] []
              P0!Stms!Range[0, 3] []
              P0!Stms!Range[1, 1] []
              P0!Stms!Range[1, 2] []
              P0!Stms!Range[1, 4] []
              P0!Stms!Range[1, 5] []
              P0!Stms!Range[2, 0] []
              P0!Stms!Range[2, 4] []
              P0!Stms!Range[3, 0] []
              P0!Stms!Range[3, 2] []
              P1!Stms!Range[0, 1] []
              P1!Stms!Range[0, 2] []
              P1!Stms!Range[1, 0] []
              P1!Stms!Range[2, 0] [] |}]

    let%expect_test "transform-list with filtering to dead code" =
      test Transform_list Act_fuzz.Path_filter.(empty |> in_dead_code_only) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[4]!If!True!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[4]!If!True!Range[0, 1] [in-dead-code]
              P0!Stms!Stm[4]!If!True!Range[1, 0] [in-dead-code]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0] [in-dead-code, in-loop]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 1] [in-dead-code, in-loop]
              P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0] [in-dead-code, in-loop]
              P1!Stms!Stm[1]!If!False!Range[0, 0] [in-dead-code]
              P1!Stms!Stm[1]!If!False!Range[0, 1] [in-dead-code]
              P1!Stms!Stm[1]!If!False!Range[1, 0] [in-dead-code] |}]

    let%expect_test "transform-list with filtering to if statements" =
      test Transform_list
        Act_fuzz.Path_filter.(
          empty |> require_end_check ~check:(Stm_class (Is, [If]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!True!Range[1, 0] [in-execute-multi]
              P0!Stms!Stm[4]!If!False!Range[0, 0] [in-execute-multi]
              P0!Stms!Stm[4]!If!True!Range[1, 0] [in-dead-code]
              P0!Stms!Range[0, 0] []
              P0!Stms!Range[1, 0] []
              P0!Stms!Range[2, 0] []
              P0!Stms!Range[3, 0] []
              P0!Stms!Range[3, 1] []
              P0!Stms!Range[3, 2] []
              P0!Stms!Range[4, 0] []
              P0!Stms!Range[5, 0] []
              P1!Stms!Stm[1]!If!False!Range[1, 0] [in-dead-code]
              P1!Stms!Range[1, 0] []
              P1!Stms!Range[1, 1] []
              P1!Stms!Range[2, 0] [] |}]

    let%expect_test "transform-list with filtering to non-labels" =
      (* TODO(@MattWindsor91): should this be excluding [0, 0]? *)
      test Transform_list
        Act_fuzz.Path_filter.(
          empty
          |> require_end_check
               ~check:(Stm_class (Is_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!True!Range[0, 0] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[0, 1] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[1, 0] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[2, 0] [in-execute-multi]
              P0!Stms!Stm[4]!If!True!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[4]!If!True!Range[0, 1] [in-dead-code]
              P0!Stms!Stm[4]!If!True!Range[1, 0] [in-dead-code]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 1] [in-dead-code, in-loop]
              P0!Stms!Range[1, 1] []
              P0!Stms!Range[2, 2] []
              P0!Stms!Range[2, 4] []
              P0!Stms!Range[3, 0] []
              P0!Stms!Range[3, 2] []
              P0!Stms!Range[3, 3] []
              P0!Stms!Range[5, 1] []
              P1!Stms!Stm[1]!If!False!Range[0, 0] [in-dead-code] |}]

    let%expect_test "transform-list with filtering to recursive non-labels" =
      test Transform_list
        Act_fuzz.Path_filter.(
          empty
          |> require_end_check
               ~check:(Stm_class (Has_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[3]!If!True!Range[0, 0] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[0, 1] [in-execute-multi]
              P0!Stms!Stm[3]!If!True!Range[1, 0] [in-execute-multi]
              P0!Stms!Stm[4]!If!True!Range[0, 0] [in-dead-code]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 1] [in-dead-code, in-loop]
              P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0] [in-dead-code, in-loop]
              P0!Stms!Range[0, 2] []
              P0!Stms!Range[2, 0] []
              P0!Stms!Range[3, 0] []
              P0!Stms!Range[4, 1] []
              P0!Stms!Range[4, 2] []
              P0!Stms!Range[5, 1] []
              P1!Stms!Stm[1]!If!False!Range[0, 0] [in-dead-code]
              P1!Stms!Stm[1]!If!False!Range[1, 0] [in-dead-code]
              P1!Stms!Stm[1]!If!True!Range[0, 0] [in-execute-multi] |}]
  end )
