(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

let%test_module "Statement_list" =
  ( module struct
    let%test_unit "insertions into an empty list are always at index 0" =
      Test.run_exn
        ( module struct
          type t = Act_fuzz.Path.stm_list [@@deriving sexp]

          let quickcheck_generator =
            Option.value_exn
              (Act_fuzz.Path_producers.Statement_list.try_gen_insert_stm [])

          let quickcheck_shrinker = Shrinker.atomic
        end )
        ~f:(function
          | Act_fuzz.Path.Insert 0 -> () | _ -> failwith "Unexpected path")

    let%test_module "sample path output on example code" =
      ( module struct
        (* These test endpoints mainly serve to provide early warning if the
           way that path generators work has changed. *)

        let print_sample (generator : Act_fuzz.Path.stm_list Generator.t) :
            unit =
          Act_utils.My_quickcheck.print_sample
            ( module struct
              type t = Act_fuzz.Path.stm_list [@@deriving compare, sexp]

              let quickcheck_generator = generator

              let quickcheck_observer = Observer.opaque

              let quickcheck_shrinker = Shrinker.atomic
            end )

        let%expect_test "gen_insert_stm" =
          print_sample
            (Option.value_exn
               (Act_fuzz.Path_producers.Statement_list.try_gen_insert_stm
                  (Lazy.force Subject.Test_data.body_stms))) ;
          [%expect
            {|
              (Insert 0)
              (Insert 1)
              (Insert 2)
              (Insert 3)
              (In_stm 3 (In_if (In_block false (Insert 0))))
              (In_stm 3 (In_if (In_block true (Insert 1)))) |}]

        let%expect_test "try_gen_transform_stm with no filtering" =
          let gen =
            Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm
              (Lazy.force Subject.Test_data.body_stms)
          in
          print_sample (Option.value_exn gen) ;
          [%expect
            {|
            (In_stm 0 This_stm)
            (In_stm 1 This_stm)
            (In_stm 2 This_stm)
            (In_stm 3 (In_if (In_block true (In_stm 0 This_stm))))
            (In_stm 3 This_stm) |}]

        let%expect_test "try_gen_transform_stm with filtering to if \
                         statements" =
          let gen =
            Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm
              (Lazy.force Subject.Test_data.body_stms)
              ~predicate:Act_c_mini.Statement.is_if_statement
          in
          print_sample (Option.value_exn gen) ;
          [%expect {| (In_stm 3 This_stm) |}]

        let%expect_test "gen_transform_stm_list" =
          let gen =
            Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm_list
              (Lazy.force Subject.Test_data.body_stms)
          in
          print_sample (Option.value_exn gen) ;
          [%expect
            {|
              (In_stm 3 (In_if (In_block true (On_stm_range 0 0))))
              (On_stm_range 0 0)
              (On_stm_range 0 1)
              (On_stm_range 0 3)
              (On_stm_range 1 0)
              (On_stm_range 1 2)
              (On_stm_range 2 0)
              (On_stm_range 2 1) |}]
      end )
  end )
