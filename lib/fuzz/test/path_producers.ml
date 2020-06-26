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
        Act_fir.(
          Statement.flow
            (Flow_block.while_loop ~cond:Act_fir.Expression.falsehood
               ~body:(Act_fuzz.Subject.Block.make_generated ())
               ~kind:Do_while))
      in
      let gen =
        Act_fuzz.Path_producers.Statement.try_gen_insert_stm example
      in
      Or_error.iter_error gen
        ~f:(Fmt.pr "@[unexpected error:@ %a@]@." Error.pp) ;
      [%expect]
  end )

let%test_module "Block" =
  ( module struct
    let%test_unit "insertions into an empty block are always at index 0" =
      Test.run_exn
        ( module struct
          type t = Act_fuzz.Path.Stms.t [@@deriving sexp]

          let quickcheck_generator =
            Or_error.ok_exn
              (Act_fuzz.Path_producers.Block.try_gen_insert_stm
                 (Act_fuzz.Subject.Block.make_existing ()))

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
          Act_utils.My_quickcheck.print_sample ~test_count:25
            ~printer:(Fmt.pr "@[%a@]@." Act_fuzz.Path.Stms.pp)
            ( module struct
              type t = Act_fuzz.Path.stm_list [@@deriving compare, sexp]

              let quickcheck_generator = generator

              let quickcheck_observer = Observer.opaque

              let quickcheck_shrinker = Shrinker.atomic
            end )

        let test
            (gen :
                 Act_fuzz.Subject.Block.t
              -> Act_fuzz.Path.Stms.t Act_fuzz.Opt_gen.t) : unit =
          let statements = Lazy.force Subject.Test_data.body_stms in
          let block = Act_fuzz.Subject.Block.make_existing ~statements () in
          print_sample (Or_error.ok_exn (gen block))

        let%expect_test "try_gen_insert_stm with no filtering" =
          test Act_fuzz.Path_producers.Block.try_gen_insert_stm ;
          [%expect
            {|
              Insert[0]
              Insert[1]
              Insert[2]
              Insert[3]
              Insert[4]
              Insert[5]
              Insert[6]
              Stm[3]!If!False!Insert[0]
              Stm[3]!If!True!Insert[0]
              Stm[3]!If!True!Insert[1]
              Stm[4]!If!False!Insert[0]
              Stm[4]!If!True!Insert[0]
              Stm[5]!Flow-block!Body!Insert[0]
              Stm[5]!Flow-block!Body!Insert[1] |}]

        let%expect_test "try_gen_insert_stm with dead-code filtering" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_insert_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
          [%expect
            {|
            Stm[3]!If!False!Insert[0]
            Stm[4]!If!True!Insert[0]
            Stm[4]!If!True!Insert[1]
            Stm[5]!Flow-block!Body!Insert[0]
            Stm[5]!Flow-block!Body!Insert[1] |}]

        let%expect_test "try_gen_transform_stm with no filtering" =
          test Act_fuzz.Path_producers.Block.try_gen_transform_stm ;
          [%expect
            {|
            Stm[0]!This
            Stm[1]!This
            Stm[2]!This
            Stm[3]!If!True!Stm[0]!This
            Stm[3]!If!True!Stm[1]!This
            Stm[3]!This
            Stm[4]!If!True!Stm[0]!This
            Stm[4]!This
            Stm[5]!Flow-block!Body!Stm[0]!This
            Stm[5]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to if \
                         statements" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm
               ~filter:
                 Act_fuzz.Path_filter.(
                   empty |> require_end_check ~check:(Is_of_class [If]))) ;
          [%expect {|
            Stm[3]!This
            Stm[4]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to dead code" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
          [%expect
            {|
              Stm[4]!If!True!Stm[0]!This
              Stm[5]!Flow-block!Body!Stm[0]!This |}]

        let%expect_test "try_gen_transform_stm with filtering to loops" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm
               ~filter:Act_fuzz.Path_filter.(empty |> in_loop_only)) ;
          [%expect {| Stm[5]!Flow-block!Body!Stm[0]!This |}]

        let%expect_test "try_gen_transform_stm_list" =
          test Act_fuzz.Path_producers.Block.try_gen_transform_stm_list ;
          [%expect
            {|
              Stm[3]!If!False!Range[0, 0]
              Stm[3]!If!True!Range[0, 1]
              Stm[3]!If!True!Range[0, 2]
              Stm[3]!If!True!Range[1, 0]
              Stm[3]!If!True!Range[2, 0]
              Stm[4]!If!False!Range[0, 0]
              Stm[4]!If!True!Range[0, 0]
              Stm[4]!If!True!Range[1, 0]
              Stm[5]!Flow-block!Body!Range[0, 0]
              Stm[5]!Flow-block!Body!Range[0, 1]
              Range[2, 0]
              Range[3, 0]
              Range[3, 1]
              Range[5, 1]
              Range[6, 0] |}]

        let%expect_test "try_gen_transform_stm_list with filtering to dead \
                         code" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm_list
               ~filter:Act_fuzz.Path_filter.(empty |> in_dead_code_only)) ;
          [%expect
            {|
              Stm[3]!If!False!Range[0, 0]
              Stm[4]!If!True!Range[0, 0]
              Stm[4]!If!True!Range[0, 1]
              Stm[4]!If!True!Range[1, 0]
              Stm[5]!Flow-block!Body!Range[0, 0]
              Stm[5]!Flow-block!Body!Range[0, 1]
              Stm[5]!Flow-block!Body!Range[1, 0] |}]

        let%expect_test "try_gen_transform_stm_list with filtering to if \
                         statements" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm_list
               ~filter:
                 Act_fuzz.Path_filter.(
                   empty |> require_end_check ~check:(Is_of_class [If]))) ;
          [%expect
            {|
              Stm[3]!If!False!Range[0, 0]
              Stm[3]!If!True!Range[1, 0]
              Stm[3]!If!True!Range[2, 0]
              Stm[4]!If!False!Range[0, 0]
              Stm[4]!If!True!Range[1, 0]
              Stm[5]!Flow-block!Body!Range[0, 0]
              Stm[5]!Flow-block!Body!Range[1, 0]
              Range[2, 0]
              Range[3, 0]
              Range[3, 1]
              Range[5, 0]
              Range[6, 0] |}]

        let%expect_test "try_gen_transform_stm_list with filtering to \
                         non-labels" =
          test
            (Act_fuzz.Path_producers.Block.try_gen_transform_stm_list
               ~filter:
                 Act_fuzz.Path_filter.(
                   empty
                   |> require_end_check
                        ~check:(Is_not_of_class [Prim (Some Label)]))) ;
          [%expect
            {|
              Stm[3]!If!False!Range[0, 0]
              Stm[3]!If!True!Range[1, 0]
              Stm[3]!If!True!Range[2, 0]
              Stm[4]!If!False!Range[0, 0]
              Stm[4]!If!True!Range[0, 0]
              Stm[4]!If!True!Range[1, 0]
              Stm[5]!Flow-block!Body!Range[0, 0]
              Stm[5]!Flow-block!Body!Range[0, 1]
              Range[2, 0]
              Range[5, 1]
              Range[6, 0] |}]
      end )
  end )

let%test_module "Program" =
  ( module struct
    let%test_module "sample path output on example code" =
      ( module struct
        let print_sample (generator : Act_fuzz.Path.Program.t Generator.t) :
            unit =
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
          test Act_fuzz.Path_producers.Test.try_gen_insert_stm ;
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
            (Act_fuzz.Path_producers.Test.try_gen_insert_stm
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
      end )
  end )
