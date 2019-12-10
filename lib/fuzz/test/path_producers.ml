(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

let%test_module "Statement" =
  ( module struct
    let%expect_test "insert-stm generation is viable inside a do-while loop"
        =
      let example =
        Act_c_mini.(
          Statement.(
            while_loop
              (While.make ~cond:Act_c_mini.Expression.falsehood
                 ~body:(Act_fuzz.Subject.Block.make_generated ())
                 ~kind:`Do_while)))
      in
      let gen =
        Act_fuzz.Path_producers.Statement.try_gen_insert_stm example
      in
      Or_error.iter_error gen
        ~f:(Fmt.pr "@[unexpected error:@ %a@]@." Error.pp) ;
      [%expect]
  end )

let%test_module "Statement_list" =
  ( module struct
    let%test_unit "insertions into an empty list are always at index 0" =
      Test.run_exn
        ( module struct
          type t = Act_fuzz.Path.stm_list [@@deriving sexp]

          let quickcheck_generator =
            Or_error.ok_exn
              (Act_fuzz.Path_producers.Statement_list.try_gen_insert_stm [])

          let quickcheck_shrinker = Shrinker.atomic
        end )
        ~f:(function
          | Act_fuzz.Path.Insert 0 -> () | _ -> failwith "Unexpected path")

    let%test_module "sample path output on example code" =
      ( module struct
        (* These test endpoints mainly serve to provide early warning if the
           way that path generators work has changed. *)

        let print_sample (generator : Act_fuzz.Path.Stms.t Generator.t) :
            unit =
          Act_utils.My_quickcheck.print_sample
            ~printer:(Fmt.pr "@[%a@]@." Act_fuzz.Path.Stms.pp)
            ( module struct
              type t = Act_fuzz.Path.stm_list [@@deriving compare, sexp]

              let quickcheck_generator = generator

              let quickcheck_observer = Observer.opaque

              let quickcheck_shrinker = Shrinker.atomic
            end )

        let test
            (gen :
                 Act_fuzz.Subject.Statement.t list
              -> Act_fuzz.Path.stm_list Act_fuzz.Opt_gen.t) : unit =
          print_sample
            (Or_error.ok_exn (gen (Lazy.force Subject.Test_data.body_stms)))

        let%expect_test "try_gen_insert_stm with no filtering" =
          test Act_fuzz.Path_producers.Statement_list.try_gen_insert_stm ;
          [%expect
            {|
              Insert[0]
              Insert[1]
              Insert[2]
              Insert[3]
              Insert[4]
              Insert[5]
              Insert[6]
              Stm[3]!If!True!Insert[1]
              Stm[4]!If!False!Insert[0]
              Stm[4]!If!True!Insert[0]
              Stm[5]!Loop!Body!Insert[0]
              Stm[5]!Loop!Body!Insert[1] |}]

        let%expect_test "try_gen_insert_stm with dead-code filtering" =
          test
            (Act_fuzz.Path_producers.Statement_list.try_gen_insert_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
          [%expect
            {|
            Stm[3]!If!False!Insert[0]
            Stm[4]!If!True!Insert[0]
            Stm[4]!If!True!Insert[1]
            Stm[5]!Loop!Body!Insert[0]
            Stm[5]!Loop!Body!Insert[1] |}]

        let%expect_test "try_gen_transform_stm with no filtering" =
          test Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm ;
          [%expect
            {|
            Stm[0]!This
            Stm[1]!This
            Stm[2]!This
            Stm[3]!If!True!Stm[0]!This
            Stm[3]!This
            Stm[4]!If!True!Stm[0]!This
            Stm[5]!Loop!Body!Stm[0]!This
            Stm[5]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to if \
                         statements" =
          test
            (Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm
               ~filter:
                 Act_fuzz.Path_filter.(empty |> final_if_statements_only)) ;
          [%expect {|
            Stm[3]!This
            Stm[4]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to dead code" =
          test
            (Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
          [%expect
            {|
              Stm[4]!If!True!Stm[0]!This
              Stm[5]!Loop!Body!Stm[0]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to loops" =
          test
            (Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_loop_only)) ;
          [%expect {| Stm[5]!Loop!Body!Stm[0]!This |}]

        let%expect_test "gen_transform_stm_list" =
          let gen =
            Act_fuzz.Path_producers.Statement_list.try_gen_transform_stm_list
              (Lazy.force Subject.Test_data.body_stms)
          in
          print_sample (Or_error.ok_exn gen) ;
          [%expect
            {|
              Stm[3]!If!False!Range[0, 0]
              Stm[3]!If!True!Range[0, 0]
              Stm[3]!If!True!Range[1, 0]
              Stm[4]!If!False!Range[0, 0]
              Stm[4]!If!True!Range[0, 0]
              Stm[4]!If!True!Range[1, 0]
              Stm[5]!Loop!Body!Range[0, 0]
              Range[3, 0]
              Range[3, 1]
              Range[5, 0]
              Range[6, 0] |}]
      end )
  end )
