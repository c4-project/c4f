(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
open Stdio

let%test_module "Statement_list" =
  ( module struct
    let%test_unit "insertions into an empty list are always at index 0" =
      Test.run_exn
        ( module struct
          type t = Act_fuzz.Path_shapes.stm_list [@@deriving sexp]

          let quickcheck_generator =
            Act_fuzz.Path.Statement_list.gen_insert_stm []

          let quickcheck_shrinker = Shrinker.atomic
        end )
        ~f:(function
          | Act_fuzz.Path_shapes.Insert 0 ->
              ()
          | _ ->
              failwith "Unexpected path")

    let%test_module "sample path output on example code" =
      ( module struct
        (* These test endpoints mainly serve to provide early warning if the
           way that path generators work has changed. *)

        let print_sample
            (generator : Act_fuzz.Path_shapes.stm_list Generator.t) : unit =
          Act_utils.My_quickcheck.print_sample
            ( module struct
              type t = Act_fuzz.Path_shapes.stm_list
              [@@deriving compare, sexp]

              let quickcheck_generator = generator

              let quickcheck_observer = Observer.opaque

              let quickcheck_shrinker = Shrinker.atomic
            end )

        let%expect_test "gen_insert_stm" =
          print_sample
            (Act_fuzz.Path.Statement_list.gen_insert_stm
               (Lazy.force Subject.Test_data.body_stms)) ;
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
            Act_fuzz.Path.Statement_list.try_gen_transform_stm
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
            Act_fuzz.Path.Statement_list.try_gen_transform_stm
              (Lazy.force Subject.Test_data.body_stms)
              ~predicate:Act_c_mini.Statement.is_if_statement
          in
          print_sample (Option.value_exn gen) ;
          [%expect {| (In_stm 3 This_stm) |}]

        let%expect_test "gen_transform_stm_list" =
          let gen =
            Act_fuzz.Path.Statement_list.try_gen_transform_stm_list
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

    let%test_module "paths applied to example code" =
      ( module struct
        module F = Act_fuzz
        module Stm = Act_c_mini.Statement

        let insert_path : F.Path_shapes.stm_list = Insert 2

        let insert_at_end_path : F.Path_shapes.stm_list = Insert 3

        let in_stm_path : F.Path_shapes.stm_list = In_stm (2, This_stm)

        let on_stm_range_path : F.Path_shapes.stm_list = On_stm_range (1, 2)

        let example_stm : F.Metadata.t Stm.t =
          Act_c_mini.(
            Stm.atomic_store
              (Atomic_store.make ~mo:Mem_order.Seq_cst
                 ~src:(Expression.int_lit 9001)
                 ~dst:(Address.of_variable_str_exn "y")))

        (* TODO(@MattWindsor91): generalise this? *)
        let pp_statement : F.Metadata.t Stm.t Fmt.t =
          Fmt.using Act_c_mini.Reify.stm Act_c_lang.Ast.Stm.pp

        let test (stms : F.Metadata.t Stm.t list Or_error.t) : unit =
          Fmt.(
            pr "@[<v>%a@]@."
              (result ~ok:(list ~sep:sp (box pp_statement)) ~error:Error.pp)
              stms)

        let body_stms : F.Metadata.t Stm.t list Lazy.t =
          Lazy.(
            Subject.Test_data.body_stms
            >>| List.map
                  ~f:(Stm.On_meta.map ~f:(Fn.const F.Metadata.existing)))

        let%test_module "insert_stm" =
          ( module struct
            let test_insert (path : F.Path_shapes.stm_list) : unit =
              test
                (F.Path.Statement_list.insert_stm path ~to_insert:example_stm
                   ~target:(Lazy.force body_stms))

            let%expect_test "insert onto statement (invalid)" =
              test_insert in_stm_path ;
              [%expect {| ("Can't insert statement here" (path This_stm)) |}]

            let%expect_test "insert onto range (invalid)" =
              test_insert on_stm_range_path ;
              [%expect
                {|
          ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:160:65)
           (context insert_stm) (path (On_stm_range 1 2))) |}]

            let%expect_test "insert into list" =
              test_insert insert_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); } |}]

            let%expect_test "insert onto end of list" =
              test_insert insert_at_end_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); } |}]
          end )

        let%test_module "transform_stm" =
          ( module struct
            let test_transform (path : F.Path_shapes.stm_list) : unit =
              test
                (F.Path.Statement_list.transform_stm path
                   ~f:(Fn.const (Or_error.return example_stm))
                   ~target:(Lazy.force body_stms))

            let%expect_test "transform a statement" =
              test_transform in_stm_path ;
              [%expect
                {|
                  atomic_store_explicit(x, 42, memory_order_seq_cst);
                  ;
                  atomic_store_explicit(y, 9001, memory_order_seq_cst);
                  if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); } |}]

            let%expect_test "transform a range" =
              test_transform on_stm_range_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); } |}]

            let%expect_test "transform an insertion (invalid)" =
              test_transform insert_path ;
              [%expect
                {|
          ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:171:68)
           (context transform_stm) (path (Insert 2))) |}]
          end )

        let%test_module "transform_stm_list" =
          ( module struct
            let iffify (statements : F.Metadata.t Stm.t list) :
                F.Metadata.t Stm.t list Or_error.t =
              Or_error.return
                [ Stm.if_stm
                    (Stm.If.make
                       ~cond:(Act_c_mini.Expression.bool_lit true)
                       ~t_branch:
                         (F.Subject.Block.make_generated ~statements ())
                       ~f_branch:(F.Subject.Block.make_generated ())) ]

            let test_transform_list (path : F.Path_shapes.stm_list) : unit =
              test
                (F.Path.Statement_list.transform_stm_list path ~f:iffify
                   ~target:(Lazy.force body_stms))

            let%expect_test "try to list-transform a statement (invalid)" =
              test_transform_list in_stm_path ;
              [%expect
                {| ("Can't transform multiple statements here" (path This_stm)) |}]

            let%expect_test "list-transform a range" =
              test_transform_list on_stm_range_path ;
              [%expect
                {|
                  atomic_store_explicit(x, 42, memory_order_seq_cst);
                  if (true) { ; atomic_store_explicit(y, foo, memory_order_relaxed); }
                  if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); } |}]

            let%expect_test "list-transform an insertion (invalid)" =
              test_transform_list insert_path ;
              [%expect
                {|
                  ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:183:16)
                   (context transform_stm_list) (path (Insert 2))) |}]

            let%test_unit "generator over stm-list produces valid paths" =
              Test.run_exn
                ( module struct
                  type t = F.Path_shapes.stm_list [@@deriving sexp]

                  let quickcheck_generator =
                    Option.value_exn ~here:[%here]
                      (F.Path.Statement_list.try_gen_transform_stm_list
                         (Lazy.force body_stms))

                  let quickcheck_shrinker =
                    (* for now *)
                    Shrinker.atomic
                end )
                ~f:(fun path ->
                  [%test_result: unit Or_error.t] ~here:[[%here]]
                    ~expect:(Or_error.return ())
                    (Or_error.map ~f:(Fn.const ())
                       (F.Path.Statement_list.transform_stm_list path
                          ~f:Or_error.return ~target:(Lazy.force body_stms))))
          end )
      end )
  end )

let%test_module "s-expression serialisation" =
  ( module struct
    open Act_fuzz.Path_shapes

    let test (x : program) : unit = print_s [%sexp (x : program)]

    let%expect_test "this-statement example" =
      test
        (in_func 0
           (in_stms (in_stm 2 (in_if (in_block true (in_stm 5 this_stm)))))) ;
      [%expect
        {| (In_func 0 (In_stms (In_stm 2 (In_if (In_block true (In_stm 5 This_stm)))))) |}]

    let%expect_test "insert-statement example" =
      test (in_func 3 (in_stms (insert 0))) ;
      [%expect {| (In_func 3 (In_stms (Insert 0))) |}]

    let%expect_test "if-statement conditional example" =
      test (in_func 2 (in_stms (in_stm 9 (in_if this_cond)))) ;
      [%expect {| (In_func 2 (In_stms (In_stm 9 (In_if This_cond)))) |}]
  end )
