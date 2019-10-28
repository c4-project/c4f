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
                 ~dst:(Address.of_variable (Act_common.C_id.of_string "y"))))

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
            Subject.Example.body_stms
            >>| List.map
                  ~f:(Stm.On_meta.map ~f:(Fn.const F.Metadata.existing)))

        let%test_module "insert_stm" =
          ( module struct
            let test_insert (path : F.Path_shapes.stm_list) : unit =
              test
                (F.Path.Statement_list.insert_stm path
                   ~to_insert:example_stm ~target:(Lazy.force body_stms))

            let%expect_test "insert onto statement (invalid)" =
              test_insert in_stm_path ;
              [%expect
                {| ("Can't insert statement here" (path This_stm)) |}]

            let%expect_test "insert onto range (invalid)" =
              test_insert on_stm_range_path ;
              [%expect
                {|
          ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:148:65)
           (context insert_stm) (path (On_stm_range 1 2))) |}]

            let%expect_test "insert into list" =
              test_insert insert_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed); |}]

            let%expect_test "insert onto end of list" =
              test_insert insert_at_end_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          atomic_store_explicit(y, 9001, memory_order_seq_cst); |}]
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
                  atomic_store_explicit(y, 9001, memory_order_seq_cst); |}]

            let%expect_test "transform a range" =
              test_transform on_stm_range_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst); |}]

            let%expect_test "transform an insertion (invalid)" =
              test_transform insert_path ;
              [%expect
                {|
          ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:159:68)
           (context transform_stm) (path (Insert 2))) |}]
          end )

        let%test_module "transform_stm_list" =
          ( module struct
            type stm = F.Metadata.t Stm.t

            let make_block (statements : stm list) :
                (F.Metadata.t, stm) Act_c_mini.Block.t =
              Act_c_mini.Block.make ~metadata:F.Metadata.generated
                ~statements ()

            let iffify (statements : F.Metadata.t Stm.t list) :
                F.Metadata.t Stm.t list Or_error.t =
              Or_error.return
                [ Stm.if_stm
                    (Stm.If.make
                       ~cond:(Act_c_mini.Expression.bool_lit true)
                       ~t_branch:(make_block statements)
                       ~f_branch:(make_block [])) ]

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
                  if (true) { ; atomic_store_explicit(y, foo, memory_order_relaxed); } |}]

            let%expect_test "list-transform an insertion (invalid)" =
              test_transform_list insert_path ;
              [%expect
                {|
                  ("Can't use this statement-list path here" (here lib/fuzz/src/path.ml:171:16)
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
