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
    let print_sample (generator : Act_fuzz.Path.Program.t Generator.t) : unit
        =
      Act_utils.My_quickcheck.print_sample
        ~printer:(Fmt.pr "@[%a@]@." Act_fuzz.Path.Program.pp)
        ( module struct
          type t = Act_fuzz.Path.Program.t [@@deriving compare, sexp]

          let quickcheck_generator = generator

          let quickcheck_observer = Observer.opaque

          let quickcheck_shrinker = Shrinker.atomic
        end )

    let test
        (gen :
             Act_fuzz.Subject.Test.t
          -> Act_fuzz.Path.Program.t Act_fuzz.Opt_gen.t) : unit =
      let test = Lazy.force Subject.Test_data.test in
      print_sample (Or_error.ok_exn (gen test))

    let%expect_test "try_gen_insert_stm with no filtering" =
      test Act_fuzz.Path_producers.try_gen_insert_stm ;
      [%expect
        {|
              P0!Stms!Insert[4]
              P0!Stms!Insert[6]
              P0!Stms!Stm[3]!If!True!Insert[0]
              P0!Stms!Stm[3]!If!True!Insert[1]
              P0!Stms!Stm[5]!Flow-block!Body!Insert[1]
              P1!Stms!Insert[0]
              P1!Stms!Insert[1]
              P1!Stms!Insert[2]
              P1!Stms!Stm[1]!If!False!Insert[0]
              P1!Stms!Stm[1]!If!False!Insert[1]
              P1!Stms!Stm[1]!If!True!Insert[0] |}]

    let%expect_test "try_gen_insert_stm with thread filtering" =
      test
        (Act_fuzz.Path_producers.try_gen_insert_stm
           ~filter:
             Act_fuzz.Path_filter.(
               empty
               |> in_threads_only ~threads:(Set.singleton (module Int) 1))) ;
      [%expect
        {|
              P1!Stms!Insert[0]
              P1!Stms!Insert[1]
              P1!Stms!Insert[2]
              P1!Stms!Stm[1]!If!False!Insert[1]
              P1!Stms!Stm[1]!If!True!Insert[0] |}]

    let%expect_test "try_gen_insert_stm with dead-code filtering" =
      test
        (Act_fuzz.Path_producers.try_gen_insert_stm
           ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
      [%expect
        {|
            P0!Stms!Stm[3]!If!False!Insert[0]
            P0!Stms!Stm[4]!If!True!Insert[0]
            P0!Stms!Stm[4]!If!True!Insert[1]
            P0!Stms!Stm[5]!Flow-block!Body!Insert[0]
            P0!Stms!Stm[5]!Flow-block!Body!Insert[1]
            P1!Stms!Stm[1]!If!False!Insert[0]
            P1!Stms!Stm[1]!If!False!Insert[1] |}]

    let%expect_test "try_gen_transform_stm with no filtering" =
      test Act_fuzz.Path_producers.try_gen_transform_stm ;
      [%expect
        {|
            P0!Stms!Stm[1]!This
            P0!Stms!Stm[2]!This
            P0!Stms!Stm[3]!This
            P0!Stms!Stm[4]!This
            P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This
            P0!Stms!Stm[5]!This
            P1!Stms!Stm[0]!This
            P1!Stms!Stm[1]!If!False!Stm[0]!This
            P1!Stms!Stm[1]!This |}]

    let%expect_test "try_gen_transform_stm with filtering to if statements" =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm
           ~filter:
             Act_fuzz.Path_filter.(
               empty |> require_end_check ~check:(Is_of_class [If]))) ;
      [%expect
        {|
            P0!Stms!Stm[3]!This
            P0!Stms!Stm[4]!This
            P1!Stms!Stm[1]!This |}]

    let%expect_test "try_gen_transform_stm with filtering to dead code" =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm
           ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
      [%expect
        {|
              P0!Stms!Stm[4]!If!True!Stm[0]!This
              P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This
              P1!Stms!Stm[1]!If!False!Stm[0]!This |}]

    let%expect_test "try_gen_transform_stm with filtering to loops" =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm
           ~filter:Act_fuzz.Path_filter.(empty |> in_loop_only)) ;
      [%expect {| P0!Stms!Stm[5]!Flow-block!Body!Stm[0]!This |}]

    let%expect_test "try_gen_transform_stm_list" =
      test Act_fuzz.Path_producers.try_gen_transform_stm_list ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0]
              P0!Stms!Stm[3]!If!True!Range[1, 1]
              P0!Stms!Stm[4]!If!False!Range[0, 0]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0]
              P0!Stms!Range[1, 4]
              P0!Stms!Range[2, 0]
              P0!Stms!Range[6, 0]
              P1!Stms!Stm[1]!If!False!Range[0, 1]
              P1!Stms!Stm[1]!If!True!Range[0, 0]
              P1!Stms!Range[0, 0]
              P1!Stms!Range[1, 1]
              P1!Stms!Range[2, 0] |}]

    let%expect_test "try_gen_transform_stm_list with filtering to dead code"
        =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm_list
           ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0]
              P0!Stms!Stm[4]!If!True!Range[0, 0]
              P0!Stms!Stm[4]!If!True!Range[0, 1]
              P0!Stms!Stm[4]!If!True!Range[1, 0]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 1]
              P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0]
              P1!Stms!Stm[1]!If!False!Range[0, 0]
              P1!Stms!Stm[1]!If!False!Range[0, 1]
              P1!Stms!Stm[1]!If!False!Range[1, 0] |}]

    let%expect_test "try_gen_transform_stm_list with filtering to if \
                     statements" =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm_list
           ~filter:
             Act_fuzz.Path_filter.(
               empty |> require_end_check ~check:(Is_of_class [If]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0]
              P0!Stms!Stm[4]!If!True!Range[0, 0]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0]
              P0!Stms!Stm[5]!Flow-block!Body!Range[1, 0]
              P0!Stms!Range[2, 0]
              P0!Stms!Range[6, 0]
              P1!Stms!Stm[1]!If!False!Range[0, 0]
              P1!Stms!Stm[1]!If!False!Range[1, 0]
              P1!Stms!Stm[1]!If!True!Range[0, 0]
              P1!Stms!Range[0, 0]
              P1!Stms!Range[1, 1]
              P1!Stms!Range[2, 0] |}]

    let%expect_test "try_gen_transform_stm_list with filtering to non-labels"
        =
      test
        (Act_fuzz.Path_producers.try_gen_transform_stm_list
           ~filter:
             Act_fuzz.Path_filter.(
               empty
               |> require_end_check
                    ~check:(Is_not_of_class [Prim (Some Label)]))) ;
      [%expect
        {|
              P0!Stms!Stm[3]!If!False!Range[0, 0]
              P0!Stms!Stm[3]!If!True!Range[0, 0]
              P0!Stms!Stm[4]!If!True!Range[0, 0]
              P0!Stms!Stm[5]!Flow-block!Body!Range[0, 0]
              P0!Stms!Range[1, 4]
              P0!Stms!Range[2, 1]
              P0!Stms!Range[6, 0]
              P1!Stms!Stm[1]!If!False!Range[0, 1]
              P1!Stms!Stm[1]!If!True!Range[0, 0]
              P1!Stms!Range[0, 0]
              P1!Stms!Range[1, 1]
              P1!Stms!Range[2, 0] |}]
  end )
