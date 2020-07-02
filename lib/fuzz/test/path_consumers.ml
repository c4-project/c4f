(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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
        open struct
          module F = Act_fuzz
          module Stm = Act_fir.Statement_traverse
          module P = Subject.Test_data.Path
        end

        let test (path : F.Path.Program.t Lazy.t)
            ~(f :
                  F.Path.Program.t
               -> target:F.Subject.Test.t
               -> F.Subject.Test.t Or_error.t) : unit =
          let vars = F.State.vars (Lazy.force Subject.Test_data.state) in
          Fmt.(
            pr "@[<v>%a@]@."
              (Act_utils.My_format.pp_or_error
                 Action.Test_utils.(using (Fn.flip reify_test vars) pp_tu))
              (f (Lazy.force path)
                 ~target:(Lazy.force Subject.Test_data.test)))

        let example_stm : F.Subject.Statement.t =
          Act_fir.(
            Statement.prim F.Metadata.generated
              (Prim_statement.atomic_store
                 (Atomic_store.make ~mo:Mem_order.Seq_cst
                    ~src:(Expression.int_lit 9001)
                    ~dst:(Address.of_variable_str_exn "y"))))

        let%test_module "insert_stm" =
          ( module struct
            let test_insert : F.Path.Program.t Lazy.t -> unit =
              test ~f:(F.Path_consumers.insert_stm ~to_insert:example_stm)

            let%expect_test "insert onto statement (invalid)" =
              test_insert P.in_stm ;
              [%expect {| ("Can't insert statement here" (path This_stm)) |}]

            let%expect_test "insert onto range (invalid)" =
              test_insert P.surround_atomic ;
              [%expect
                {|
          ("Can't use this statement-list path here"
           (here lib/fuzz/src/path_consumers.ml:227:65) (context insert_stm)
           (path (On_range 0 2))) |}]

            let%expect_test "insert into list" =
              test_insert P.insert_live ;
              [%expect
                {|
          void
          P0(atomic_int *x, atomic_int *y)
          {
              atomic_int r0 = 4004;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              ;
              atomic_store_explicit(y, 9001, memory_order_seq_cst);
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (foo == y)
              { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
              if (false)
              {
                  atomic_store_explicit(y,
                                        atomic_load_explicit(x, memory_order_seq_cst),
                                        memory_order_seq_cst);
              }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
          }

          void
          P1(atomic_int *x, atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } } |}]

            let%expect_test "insert onto end of list" =
              test_insert P.insert_end ;
              [%expect
                {|
          void
          P0(atomic_int *x, atomic_int *y)
          {
              atomic_int r0 = 4004;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              ;
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (foo == y)
              { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
              if (false)
              {
                  atomic_store_explicit(y,
                                        atomic_load_explicit(x, memory_order_seq_cst),
                                        memory_order_seq_cst);
              }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
              atomic_store_explicit(y, 9001, memory_order_seq_cst);
          }

          void
          P1(atomic_int *x, atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } } |}]
          end )

        let%test_module "transform_stm" =
          ( module struct
            let test_transform : F.Path.Program.t Lazy.t -> unit =
              test
                ~f:
                  (F.Path_consumers.transform_stm
                     ~f:(Fn.const (Ok example_stm)))

            let%expect_test "transform a statement" =
              test_transform P.in_stm ;
              [%expect
                {|
                  void
                  P0(atomic_int *x, atomic_int *y)
                  {
                      atomic_int r0 = 4004;
                      atomic_store_explicit(x, 42, memory_order_seq_cst);
                      ;
                      atomic_store_explicit(y, 9001, memory_order_seq_cst);
                      if (foo == y)
                      { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
                      if (false)
                      {
                          atomic_store_explicit(y,
                                                atomic_load_explicit(x, memory_order_seq_cst),
                                                memory_order_seq_cst);
                      }
                      do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
                      5);
                  }

                  void
                  P1(atomic_int *x, atomic_int *y)
                  { loop: ; if (true) {  } else { goto loop; } } |}]

            let%expect_test "transform a range" =
              test_transform P.surround_atomic ;
              [%expect
                {|
          void
          P0(atomic_int *x, atomic_int *y)
          {
              atomic_int r0 = 4004;
              atomic_store_explicit(y, 9001, memory_order_seq_cst);
              atomic_store_explicit(y, 9001, memory_order_seq_cst);
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (foo == y)
              { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
              if (false)
              {
                  atomic_store_explicit(y,
                                        atomic_load_explicit(x, memory_order_seq_cst),
                                        memory_order_seq_cst);
              }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
          }

          void
          P1(atomic_int *x, atomic_int *y)
          { loop: ; if (true) {  } else { goto loop; } } |}]

            let%expect_test "transform an insertion (invalid)" =
              test_transform P.insert_live ;
              [%expect
                {|
          ("Can't use this statement-list path here"
           (here lib/fuzz/src/path_consumers.ml:246:68) (context transform_stm)
           (path (Insert 2))) |}]
          end )

        let%test_module "transform_stm_list" =
          ( module struct
            let iffify (statements : F.Subject.Statement.t list) :
                F.Subject.Statement.t list Or_error.t =
              Or_error.return
                [ Act_fir.Statement.if_stm
                    (Act_fir.If.make
                       ~cond:(Act_fir.Expression.bool_lit true)
                       ~t_branch:
                         (F.Subject.Block.make_generated ~statements ())
                       ~f_branch:(F.Subject.Block.make_generated ())) ]

            let test_transform_list : F.Path.Program.t Lazy.t -> unit =
              test ~f:(F.Path_consumers.transform_stm_list ~f:iffify)

            let%expect_test "try to list-transform a statement (invalid)" =
              test_transform_list P.in_stm ;
              [%expect
                {| ("Can't transform multiple statements here" (path This_stm)) |}]

            let%expect_test "list-transform a range" =
              test_transform_list P.surround_atomic ;
              [%expect
                {|
                  void
                  P0(atomic_int *x, atomic_int *y)
                  {
                      atomic_int r0 = 4004;
                      if (true) { atomic_store_explicit(x, 42, memory_order_seq_cst); ; }
                      atomic_store_explicit(y, foo, memory_order_relaxed);
                      if (foo == y)
                      { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
                      if (false)
                      {
                          atomic_store_explicit(y,
                                                atomic_load_explicit(x, memory_order_seq_cst),
                                                memory_order_seq_cst);
                      }
                      do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
                      5);
                  }

                  void
                  P1(atomic_int *x, atomic_int *y)
                  { loop: ; if (true) {  } else { goto loop; } } |}]

            let%expect_test "list-transform an insertion (invalid)" =
              test_transform_list P.insert_live ;
              [%expect
                {|
                  ("Can't use this statement-list path here"
                   (here lib/fuzz/src/path_consumers.ml:259:16) (context transform_stm_list)
                   (path (Insert 2))) |}]

            let%test_unit "generator over stm-list produces valid paths" =
              Test.run_exn
                ( module struct
                  type t = F.Path.Program.t [@@deriving sexp]

                  let quickcheck_generator =
                    Or_error.ok_exn
                      (F.Path_producers.try_gen_transform_stm_list
                         (Lazy.force Subject.Test_data.test))

                  let quickcheck_shrinker =
                    (* for now *)
                    Shrinker.atomic
                end )
                ~f:(fun path ->
                  [%test_result: unit Or_error.t] ~here:[[%here]]
                    ~expect:(Or_error.return ())
                    (Or_error.map ~f:(Fn.const ())
                       (F.Path_consumers.transform_stm_list path
                          ~f:Or_error.return
                          ~target:(Lazy.force Subject.Test_data.test))) )
          end )
      end )
  end )
