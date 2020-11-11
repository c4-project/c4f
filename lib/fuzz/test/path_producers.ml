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

let print_sample (generator : Src.Path.With_meta.t Generator.t) : unit =
  Act_utils.My_quickcheck.print_sample
    ~printer:(Fmt.pr "@[%a@]@." Src.Path.With_meta.pp)
    ( module struct
      type t = Src.Path.With_meta.t [@@deriving compare, sexp]

      let quickcheck_generator = generator

      let quickcheck_observer = Observer.opaque

      let quickcheck_shrinker = Shrinker.atomic
    end )

let%test_module "sample path output on example code" =
  ( module struct
    let test (kind : Src.Path_kind.t) (filter : Src.Path_filter.t) : unit =
      let test = Lazy.force Subject.Test_data.test in
      let paths = Src.Path_producers.try_gen test ~filter ~kind in
      print_sample (Or_error.ok_exn paths)

    let%expect_test "insert with no filtering" =
      test Insert Src.Path_filter.zero ;
      [%expect
        {|
              Thread[0].Stms.Insert[0] {}[T]
              Thread[0].Stms.Insert[2] {}
              Thread[0].Stms.Insert[3] {}
              Thread[0].Stms.Insert[6] {}
              Thread[0].Stms.Stm[3].If.False.Insert[0] {}[T]
              Thread[0].Stms.Stm[3].If.False.Insert[1] {}
              Thread[0].Stms.Stm[3].If.False.Insert[2] {}[B]
              Thread[0].Stms.Stm[4].If.False.Insert[0] {}[F]
              Thread[0].Stms.Stm[5].Flow.Body.Insert[0] {in-loop}[T]
              Thread[0].Stms.Stm[6].Flow.Body.Insert[0] {in-execute-multi, in-loop}[T]
              Thread[0].Stms.Stm[6].Flow.Body.Insert[1] {in-execute-multi, in-loop}[B]
              Thread[0].Stms.Stm[7].Flow.Body.Insert[1] {in-dead-code, in-loop}[B]
              Thread[1].Stms.Insert[1] {}
              Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with top anchoring" =
      test Insert (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        Thread[0].Stms.Insert[0] {}[T]
        Thread[0].Stms.Stm[3].If.False.Insert[0] {in-dead-code}[F]
        Thread[0].Stms.Stm[3].If.False.Insert[0] {}[T]
        Thread[0].Stms.Stm[4].If.False.Insert[0] {}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Insert[0] {in-execute-multi, in-loop}[T]
        Thread[0].Stms.Stm[7].Flow.Body.Insert[0] {in-dead-code, in-loop}[T]
        Thread[1].Stms.Insert[0] {}[T]
        Thread[1].Stms.Stm[1].If.False.Insert[0] {in-dead-code}[T]
        Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with bottom anchoring" =
      test Insert (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        Thread[0].Stms.Insert[8] {}[B]
        Thread[0].Stms.Stm[3].If.False.Insert[0] {in-dead-code}[F]
        Thread[0].Stms.Stm[3].If.False.Insert[2] {}[B]
        Thread[0].Stms.Stm[4].If.False.Insert[0] {}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Insert[1] {in-execute-multi, in-loop}[B]
        Thread[0].Stms.Stm[7].Flow.Body.Insert[1] {in-dead-code, in-loop}[B]
        Thread[1].Stms.Insert[2] {}[B]
        Thread[1].Stms.Stm[1].If.False.Insert[1] {in-dead-code}[B]
        Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with full anchoring" =
      test Insert (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        Thread[0].Stms.Stm[3].If.False.Insert[0] {in-dead-code}[F]
        Thread[0].Stms.Stm[4].If.False.Insert[0] {}[F]
        Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with execute-multi filtering" =
      test Insert Src.Path_filter.(forbid_flag In_execute_multi) ;
      [%expect
        {|
              Thread[0].Stms.Insert[0] {}[T]
              Thread[0].Stms.Insert[1] {}
              Thread[0].Stms.Insert[2] {}
              Thread[0].Stms.Insert[3] {}
              Thread[0].Stms.Insert[4] {}
              Thread[0].Stms.Insert[5] {}
              Thread[0].Stms.Insert[8] {}[B]
              Thread[0].Stms.Stm[3].If.False.Insert[1] {}
              Thread[0].Stms.Stm[4].If.False.Insert[0] {}[F]
              Thread[0].Stms.Stm[4].If.False.Insert[1] {in-dead-code}[B]
              Thread[0].Stms.Stm[7].Flow.Body.Insert[0] {in-dead-code, in-loop}[T]
              Thread[1].Stms.Insert[1] {}
              Thread[1].Stms.Insert[2] {}[B]
              Thread[1].Stms.Stm[1].If.False.Insert[0] {in-dead-code}[T]
              Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with thread filtering" =
      test Insert
        Src.Path_filter.(in_threads_only (Set.singleton (module Int) 1)) ;
      [%expect
        {|
              Thread[1].Stms.Insert[0] {}[T]
              Thread[1].Stms.Insert[1] {}
              Thread[1].Stms.Insert[2] {}[B]
              Thread[1].Stms.Stm[1].If.False.Insert[0] {in-dead-code}[T]
              Thread[1].Stms.Stm[1].If.False.Insert[1] {in-dead-code}[B]
              Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "insert with dead-code required" =
      test Insert Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
            Thread[0].Stms.Stm[3].If.False.Insert[0] {in-dead-code}[F]
            Thread[0].Stms.Stm[4].If.False.Insert[0] {in-dead-code}[T]
            Thread[0].Stms.Stm[4].If.False.Insert[1] {in-dead-code}[B]
            Thread[0].Stms.Stm[7].Flow.Body.Insert[0] {in-dead-code, in-loop}[T]
            Thread[0].Stms.Stm[7].Flow.Body.Insert[1] {in-dead-code, in-loop}[B]
            Thread[1].Stms.Stm[1].If.False.Insert[1] {in-dead-code}[B] |}]

    let%expect_test "insert with dead-code forbidden" =
      test Insert Src.Path_filter.(forbid_flag In_dead_code) ;
      [%expect
        {|
            Thread[0].Stms.Insert[1] {}
            Thread[0].Stms.Insert[2] {}
            Thread[0].Stms.Insert[3] {}
            Thread[0].Stms.Insert[7] {}
            Thread[0].Stms.Stm[3].If.False.Insert[0] {}[T]
            Thread[0].Stms.Stm[3].If.False.Insert[1] {}
            Thread[0].Stms.Stm[3].If.False.Insert[2] {}[B]
            Thread[0].Stms.Stm[5].Flow.Body.Insert[1] {in-loop}[B]
            Thread[1].Stms.Insert[1] {}
            Thread[1].Stms.Insert[2] {}[B]
            Thread[1].Stms.Stm[1].If.False.Insert[0] {}[F] |}]

    let%expect_test "transform with no filtering" =
      test Transform Src.Path_filter.zero ;
      [%expect
        {|
            Thread[0].Stms.Stm[0].This {}[T]
            Thread[0].Stms.Stm[1].This {}
            Thread[0].Stms.Stm[2].This {}
            Thread[0].Stms.Stm[3].If.False.Stm[1].This {}[B]
            Thread[0].Stms.Stm[3].This {}
            Thread[0].Stms.Stm[4].This {}
            Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
            Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F]
            Thread[0].Stms.Stm[7].This {}[B]
            Thread[1].Stms.Stm[0].This {}[T]
            Thread[1].Stms.Stm[1].This {}[B] |}]

    let%expect_test "transform with top anchoring" =
      test Transform (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        Thread[0].Stms.Stm[0].This {}[T]
        Thread[0].Stms.Stm[3].If.False.Stm[0].This {}[T]
        Thread[0].Stms.Stm[4].If.False.Stm[0].This {in-dead-code}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Stm[0].This {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F]
        Thread[1].Stms.Stm[0].This {}[T]
        Thread[1].Stms.Stm[1].If.False.Stm[0].This {in-dead-code}[F] |}]

    let%expect_test "transform with bottom anchoring" =
      test Transform (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        Thread[0].Stms.Stm[3].If.False.Stm[1].This {}[B]
        Thread[0].Stms.Stm[4].If.False.Stm[0].This {in-dead-code}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Stm[0].This {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F]
        Thread[0].Stms.Stm[7].This {}[B]
        Thread[1].Stms.Stm[1].If.False.Stm[0].This {in-dead-code}[F]
        Thread[1].Stms.Stm[1].This {}[B] |}]

    let%expect_test "transform with full anchoring" =
      test Transform (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        Thread[0].Stms.Stm[4].If.False.Stm[0].This {in-dead-code}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Stm[0].This {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F]
        Thread[1].Stms.Stm[1].If.False.Stm[0].This {in-dead-code}[F] |}]

    let%expect_test "transform with filtering to if statements" =
      test Transform
        Src.Path_filter.(require_end_check (Stm_class (Is, [If]))) ;
      [%expect
        {|
            Thread[0].Stms.Stm[3].This {}
            Thread[0].Stms.Stm[4].This {}
            Thread[1].Stms.Stm[1].This {}[B] |}]

    let%expect_test "transform with filtering to dead code" =
      test Transform Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
              Thread[0].Stms.Stm[4].If.False.Stm[0].This {in-dead-code}[F]
              Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F]
              Thread[1].Stms.Stm[1].If.False.Stm[0].This {in-dead-code}[F] |}]

    let%expect_test "transform with filtering to loops" =
      test Transform Src.Path_filter.(require_flag In_loop) ;
      [%expect
        {|
        Thread[0].Stms.Stm[5].Flow.Body.Stm[0].This {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F] |}]

    let%expect_test "transform with filtering to true branches" =
      test Transform Src.Path_filter.(ends_in_block (If (Some true))) ;
      [%expect
        {|
        Thread[0].Stms.Stm[3].If.False.Stm[0].This {}[T]
        Thread[0].Stms.Stm[3].If.False.Stm[1].This {}[B]
        Thread[0].Stms.Stm[4].If.False.Stm[0].This {in-dead-code}[F] |}]

    let%expect_test "transform with filtering to false branches" =
      test Transform Src.Path_filter.(ends_in_block (If (Some false))) ;
      [%expect
        {|
        Thread[1].Stms.Stm[1].If.False.Stm[0].This {in-dead-code}[F] |}]

    let%expect_test "transform with filtering to direct loops" =
      test Transform
        Src.Path_filter.(ends_in_block (Flow (Some (Loop None)))) ;
      [%expect
        {|
        Thread[0].Stms.Stm[5].Flow.Body.Stm[0].This {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Stm[0].This {in-dead-code, in-loop}[F] |}]

    let%expect_test "transform with filtering to direct for-loops" =
      test Transform
        Src.Path_filter.(ends_in_block (Flow (Some (Loop (Some For))))) ;
      [%expect
        {|
        Thread[0].Stms.Stm[6].Flow.Body.Stm[0].This {in-execute-multi, in-loop}[F] |}]

    let%expect_test "transform-list" =
      test Transform_list Src.Path_filter.zero ;
      [%expect
        {|
              Thread[0].Stms.Stm[3].If.False.Range[0, 1] {}[T]
              Thread[0].Stms.Stm[3].If.False.Range[1, 1] {}[B]
              Thread[0].Stms.Stm[4].If.False.Range[1, 0] {in-dead-code}[B]
              Thread[0].Stms.Stm[5].Flow.Body.Range[0, 0] {in-loop}[T]
              Thread[0].Stms.Range[0, 5] {}[T]
              Thread[0].Stms.Range[0, 6] {}[T]
              Thread[0].Stms.Range[1, 0] {}
              Thread[0].Stms.Range[1, 3] {}
              Thread[0].Stms.Range[4, 1] {}
              Thread[0].Stms.Range[4, 2] {}
              Thread[0].Stms.Range[4, 3] {}
              Thread[0].Stms.Range[4, 4] {}[B]
              Thread[0].Stms.Range[5, 1] {}
              Thread[0].Stms.Range[5, 2] {}
              Thread[0].Stms.Range[5, 3] {}[B]
              Thread[0].Stms.Range[6, 1] {}
              Thread[0].Stms.Range[6, 2] {}[B]
              Thread[0].Stms.Range[7, 0] {}
              Thread[1].Stms.Range[1, 1] {}[B]
              Thread[1].Stms.Range[2, 0] {}[B] |}]

    let%expect_test "transform-list with top anchoring" =
      test Transform_list (Src.Path_filter.anchor Top) ;
      [%expect
        {|
        Thread[0].Stms.Stm[3].If.False.Range[0, 0] {}[T]
        Thread[0].Stms.Stm[3].If.False.Range[0, 1] {}[T]
        Thread[0].Stms.Stm[3].If.False.Range[0, 2] {}[F]
        Thread[0].Stms.Stm[4].If.False.Range[0, 0] {}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Range[0, 0] {in-loop}[T]
        Thread[0].Stms.Stm[6].Flow.Body.Range[0, 0] {in-execute-multi, in-loop}[T]
        Thread[0].Stms.Stm[6].Flow.Body.Range[0, 1] {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Range[0, 1] {in-dead-code, in-loop}[F]
        Thread[0].Stms.Range[0, 0] {}[T]
        Thread[0].Stms.Range[0, 2] {}[T]
        Thread[0].Stms.Range[0, 3] {}[T]
        Thread[0].Stms.Range[0, 6] {}[T]
        Thread[1].Stms.Stm[1].If.False.Range[0, 0] {}[F]
        Thread[1].Stms.Range[0, 1] {}[T] |}]

    let%expect_test "transform-list with bottom anchoring" =
      test Transform_list (Src.Path_filter.anchor Bottom) ;
      [%expect
        {|
        Thread[0].Stms.Stm[4].If.False.Range[1, 0] {in-dead-code}[B]
        Thread[0].Stms.Stm[5].Flow.Body.Range[0, 1] {in-loop}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Range[1, 0] {in-loop}[B]
        Thread[0].Stms.Stm[6].Flow.Body.Range[0, 1] {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Range[1, 0] {in-execute-multi, in-loop}[B]
        Thread[0].Stms.Stm[7].Flow.Body.Range[0, 1] {in-dead-code, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Range[1, 0] {in-dead-code, in-loop}[B]
        Thread[0].Stms.Range[7, 1] {}[B]
        Thread[0].Stms.Range[8, 0] {}[B]
        Thread[1].Stms.Stm[1].If.False.Range[1, 0] {in-dead-code}[B]
        Thread[1].Stms.Range[2, 0] {}[B] |}]

    let%expect_test "transform-list with full anchoring" =
      test Transform_list (Src.Path_filter.anchor Full) ;
      [%expect
        {|
        Thread[0].Stms.Stm[3].If.False.Range[0, 0] {in-dead-code}[F]
        Thread[0].Stms.Stm[4].If.False.Range[0, 0] {}[F]
        Thread[0].Stms.Stm[4].If.False.Range[0, 1] {in-dead-code}[F]
        Thread[0].Stms.Stm[5].Flow.Body.Range[0, 1] {in-loop}[F]
        Thread[0].Stms.Stm[6].Flow.Body.Range[0, 1] {in-execute-multi, in-loop}[F]
        Thread[0].Stms.Stm[7].Flow.Body.Range[0, 1] {in-dead-code, in-loop}[F]
        Thread[1].Stms.Stm[1].If.False.Range[0, 1] {in-dead-code}[F]
        Thread[1].Stms.Stm[1].If.False.Range[0, 0] {}[F] |}]

    let%expect_test "transform-list with filtering to dead code" =
      test Transform_list Src.Path_filter.(require_flag In_dead_code) ;
      [%expect
        {|
              Thread[0].Stms.Stm[3].If.False.Range[0, 0] {in-dead-code}[F]
              Thread[0].Stms.Stm[4].If.False.Range[0, 0] {in-dead-code}[T]
              Thread[0].Stms.Stm[4].If.False.Range[0, 1] {in-dead-code}[F]
              Thread[0].Stms.Stm[4].If.False.Range[1, 0] {in-dead-code}[B]
              Thread[0].Stms.Stm[7].Flow.Body.Range[0, 0] {in-dead-code, in-loop}[T]
              Thread[0].Stms.Stm[7].Flow.Body.Range[0, 1] {in-dead-code, in-loop}[F]
              Thread[0].Stms.Stm[7].Flow.Body.Range[1, 0] {in-dead-code, in-loop}[B]
              Thread[1].Stms.Stm[1].If.False.Range[0, 0] {in-dead-code}[T]
              Thread[1].Stms.Stm[1].If.False.Range[0, 1] {in-dead-code}[F]
              Thread[1].Stms.Stm[1].If.False.Range[1, 0] {in-dead-code}[B] |}]

    let%expect_test "transform-list with filtering to if statements" =
      test Transform_list
        Src.Path_filter.(require_end_check (Stm_class (Is, [If]))) ;
      [%expect
        {|
              Thread[0].Stms.Stm[3].If.False.Range[1, 0] {}
              Thread[0].Stms.Stm[4].If.False.Range[0, 0] {}[F]
              Thread[0].Stms.Stm[5].Flow.Body.Range[1, 0] {in-loop}[B]
              Thread[0].Stms.Stm[6].Flow.Body.Range[1, 0] {in-execute-multi, in-loop}[B]
              Thread[0].Stms.Stm[7].Flow.Body.Range[0, 0] {in-dead-code, in-loop}[T]
              Thread[0].Stms.Range[2, 0] {}
              Thread[0].Stms.Range[3, 0] {}
              Thread[0].Stms.Range[3, 1] {}
              Thread[0].Stms.Range[4, 0] {}
              Thread[0].Stms.Range[6, 0] {}
              Thread[0].Stms.Range[7, 0] {}
              Thread[0].Stms.Range[8, 0] {}[B]
              Thread[1].Stms.Stm[1].If.False.Range[0, 0] {in-dead-code}[T]
              Thread[1].Stms.Stm[1].If.False.Range[0, 0] {}[F]
              Thread[1].Stms.Range[1, 1] {}[B] |}]

    let%expect_test "transform-list with filtering to non-labels" =
      (* TODO(@MattWindsor91): should this be excluding [0, 0]? *)
      test Transform_list
        Src.Path_filter.(
          require_end_check (Stm_class (Is_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              Thread[0].Stms.Stm[3].If.False.Range[0, 0] {in-dead-code}[F]
              Thread[0].Stms.Stm[3].If.False.Range[0, 0] {}[T]
              Thread[0].Stms.Stm[3].If.False.Range[2, 0] {}[B]
              Thread[0].Stms.Stm[4].If.False.Range[0, 1] {in-dead-code}[F]
              Thread[0].Stms.Stm[5].Flow.Body.Range[0, 1] {in-loop}[F]
              Thread[0].Stms.Range[0, 1] {}[T]
              Thread[0].Stms.Range[1, 7] {}[B]
              Thread[0].Stms.Range[2, 0] {}
              Thread[0].Stms.Range[2, 4] {}
              Thread[0].Stms.Range[3, 0] {}
              Thread[0].Stms.Range[3, 1] {}
              Thread[0].Stms.Range[5, 1] {}
              Thread[0].Stms.Range[5, 3] {}[B]
              Thread[0].Stms.Range[7, 0] {}
              Thread[1].Stms.Stm[1].If.False.Range[0, 0] {in-dead-code}[T]
              Thread[1].Stms.Range[0, 0] {}[T]
              Thread[1].Stms.Range[2, 0] {}[B] |}]

    let%expect_test "transform-list with filtering to recursive non-labels" =
      test Transform_list
        Src.Path_filter.(
          require_end_check (Stm_class (Has_not_any, [Prim (Some Label)]))) ;
      [%expect
        {|
              Thread[0].Stms.Stm[3].If.False.Range[0, 0] {in-dead-code}[F]
              Thread[0].Stms.Stm[3].If.False.Range[2, 0] {}[B]
              Thread[0].Stms.Stm[4].If.False.Range[1, 0] {in-dead-code}[B]
              Thread[0].Stms.Stm[6].Flow.Body.Range[0, 0] {in-execute-multi, in-loop}[T]
              Thread[0].Stms.Stm[6].Flow.Body.Range[1, 0] {in-execute-multi, in-loop}[B]
              Thread[0].Stms.Stm[7].Flow.Body.Range[0, 0] {in-dead-code, in-loop}[T]
              Thread[0].Stms.Range[0, 0] {}[T]
              Thread[0].Stms.Range[0, 1] {}[T]
              Thread[0].Stms.Range[1, 0] {}
              Thread[0].Stms.Range[1, 1] {}
              Thread[0].Stms.Range[2, 0] {}
              Thread[0].Stms.Range[3, 0] {}
              Thread[0].Stms.Range[4, 1] {}
              Thread[0].Stms.Range[5, 0] {}
              Thread[0].Stms.Range[5, 2] {}
              Thread[0].Stms.Range[7, 0] {}
              Thread[1].Stms.Stm[1].If.False.Range[0, 0] {}[F]
              Thread[1].Stms.Range[1, 1] {}[B] |}]
  end )
