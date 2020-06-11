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
    let%test_module "paths applied to example code" =
      ( module struct
        module F = Act_fuzz
        module Stm = Act_fir.Statement

        let insert_path : F.Path.Stms.t = F.Path.Stms.insert 2

        let insert_at_end_path : F.Path.Stms.t = F.Path.Stms.insert 3

        let in_stm_path : F.Path.Stms.t = F.Path.(Stms.in_stm 2 Stm.this_stm)

        let on_stm_range_path : F.Path.Stms.t = F.Path.Stms.on_range 1 2

        let example_stm : F.Metadata.t Stm.t =
          Act_fir.(
            Statement.prim F.Metadata.generated
              (Prim_statement.atomic_store
                 (Atomic_store.make ~mo:Mem_order.Seq_cst
                    ~src:(Expression.int_lit 9001)
                    ~dst:(Address.of_variable_str_exn "y"))))

        (* TODO(@MattWindsor91): generalise this? *)
        let pp_statement : F.Subject.Statement.t Fmt.t =
          Fmt.using Act_fir.Reify_stm.reify Act_litmus_c.Ast.Stm.pp

        let test (stms : F.Subject.Block.t Or_error.t) : unit =
          Fmt.(
            pr "@[<v>%a@]@."
              (result
                 ~ok:
                   (using Act_fir.Block.statements
                      (list ~sep:sp (box pp_statement)))
                 ~error:Error.pp)
              stms)

        let body_stms : F.Subject.Statement.t list Lazy.t =
          Lazy.(
            Subject.Test_data.body_stms
            >>| List.map
                  ~f:(Stm.On_meta.map ~f:(Fn.const F.Metadata.existing)))

        let body_block : F.Subject.Block.t Lazy.t =
          Lazy.(
            body_stms
            >>| fun statements ->
            F.Subject.Block.make_existing ~statements ())

        let%test_module "insert_stm" =
          ( module struct
            let test_insert (path : F.Path.stm_list) : unit =
              test
                (F.Path_consumers.Block.insert_stm path
                   ~to_insert:example_stm ~target:(Lazy.force body_block))

            let%expect_test "insert onto statement (invalid)" =
              test_insert in_stm_path ;
              [%expect {| ("Can't insert statement here" (path This_stm)) |}]

            let%expect_test "insert onto range (invalid)" =
              test_insert on_stm_range_path ;
              [%expect
                {|
          ("Can't use this statement-list path here"
           (here lib/fuzz/src/path_consumers.ml:171:65) (context insert_stm)
           (path (On_range 1 2))) |}]

            let%expect_test "insert into list" =
              test_insert insert_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false)
          {
              atomic_store_explicit(y, atomic_load_explicit(x, memory_order_seq_cst),
                                    memory_order_seq_cst);
          }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 == 5); |}]

            let%expect_test "insert onto end of list" =
              test_insert insert_at_end_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false)
          {
              atomic_store_explicit(y, atomic_load_explicit(x, memory_order_seq_cst),
                                    memory_order_seq_cst);
          }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 == 5); |}]
          end )

        let%test_module "transform_stm" =
          ( module struct
            let test_transform (path : F.Path.stm_list) : unit =
              test
                (F.Path_consumers.Block.transform_stm path
                   ~f:(Fn.const (Or_error.return example_stm))
                   ~target:(Lazy.force body_block))

            let%expect_test "transform a statement" =
              test_transform in_stm_path ;
              [%expect
                {|
                  atomic_store_explicit(x, 42, memory_order_seq_cst);
                  ;
                  atomic_store_explicit(y, 9001, memory_order_seq_cst);
                  if (foo == y)
                  { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
                  if (false)
                  {
                      atomic_store_explicit(y, atomic_load_explicit(x, memory_order_seq_cst),
                                            memory_order_seq_cst);
                  }
                  do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 == 5); |}]

            let%expect_test "transform a range" =
              test_transform on_stm_range_path ;
              [%expect
                {|
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          atomic_store_explicit(y, 9001, memory_order_seq_cst);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false)
          {
              atomic_store_explicit(y, atomic_load_explicit(x, memory_order_seq_cst),
                                    memory_order_seq_cst);
          }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 == 5); |}]

            let%expect_test "transform an insertion (invalid)" =
              test_transform insert_path ;
              [%expect
                {|
          ("Can't use this statement-list path here"
           (here lib/fuzz/src/path_consumers.ml:190:68) (context transform_stm)
           (path (Insert 2))) |}]
          end )

        let%test_module "transform_stm_list" =
          ( module struct
            let iffify (statements : F.Metadata.t Stm.t list) :
                F.Metadata.t Stm.t list Or_error.t =
              Or_error.return
                [ Stm.if_stm
                    (Act_fir.If.make
                       ~cond:(Act_fir.Expression.bool_lit true)
                       ~t_branch:
                         (F.Subject.Block.make_generated ~statements ())
                       ~f_branch:(F.Subject.Block.make_generated ())) ]

            let test_transform_list (path : F.Path.stm_list) : unit =
              test
                (F.Path_consumers.Block.transform_stm_list path ~f:iffify
                   ~target:(Lazy.force body_block))

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
                  if (foo == y)
                  { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
                  if (false)
                  {
                      atomic_store_explicit(y, atomic_load_explicit(x, memory_order_seq_cst),
                                            memory_order_seq_cst);
                  }
                  do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 == 5); |}]

            let%expect_test "list-transform an insertion (invalid)" =
              test_transform_list insert_path ;
              [%expect
                {|
                  ("Can't use this statement-list path here"
                   (here lib/fuzz/src/path_consumers.ml:203:16) (context transform_stm_list)
                   (path (Insert 2))) |}]

            let%test_unit "generator over stm-list produces valid paths" =
              Test.run_exn
                ( module struct
                  type t = F.Path.stm_list [@@deriving sexp]

                  let quickcheck_generator =
                    Or_error.ok_exn
                      (F.Path_producers.Block.try_gen_transform_stm_list
                         (Lazy.force body_block))

                  let quickcheck_shrinker =
                    (* for now *)
                    Shrinker.atomic
                end )
                ~f:(fun path ->
                  [%test_result: unit Or_error.t] ~here:[[%here]]
                    ~expect:(Or_error.return ())
                    (Or_error.map ~f:(Fn.const ())
                       (F.Path_consumers.Block.transform_stm_list path
                          ~f:Or_error.return ~target:(Lazy.force body_block))))
          end )
      end )
  end )
