(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

open struct
  module A = Accessor
  module Fir = Act_fir
end

let%test_module "Statement_list" =
  ( module struct
    let%test_module "paths applied to example code" =
      ( module struct
        open struct
          module F = Act_fuzz
          module Stm = Fir.Statement_traverse
          module P = Subject.Test_data.Path
        end

        let test (path : F.Path.Flagged.t Lazy.t)
            ~(action : F.Path_kind.With_action.t) : unit =
          let vars = F.State.vars (Lazy.force Subject.Test_data.state) in
          Fmt.(
            pr "@[<v>%a@]@."
              (Act_utils.My_format.pp_or_error
                 Action.Test_utils.(using (Fn.flip reify_test vars) pp_tu))
              (F.Path_consumers.consume_with_flags
                 (Lazy.force Subject.Test_data.test)
                 ~path:(Lazy.force path) ~action))

        let example_stm : F.Subject.Statement.t =
          F.Subject.Statement.make_generated_prim
            Fir.(
              A.(construct (Prim_statement.atomic @> Atomic_statement.store))
                (Atomic_store.make ~mo:Mem_order.Seq_cst
                   ~src:(Expression.int_lit 9001)
                   ~dst:(Address.of_variable_str_exn "y")))

        let%test_module "insert_stm" =
          ( module struct
            let test_insert : F.Path.Flagged.t Lazy.t -> unit =
              test ~action:(Insert [example_stm])

            let%expect_test "insert onto statement (invalid)" =
              test_insert P.in_stm_flagged ;
              [%expect
                {|
                ("Unexpected kind of action associated with this path" (got insert)
                 (want transform)) |}]

            let%expect_test "insert onto range (invalid)" =
              test_insert P.surround_atomic ;
              [%expect
                {|
          ("Unexpected kind of action associated with this path" (got insert)
           (want transform_list)) |}]

            let%expect_test "insert into list" =
              test_insert P.insert_live ;
              [%expect
                {|
          void
          P0(atomic_int *x, atomic_int *y)
          {
              atomic_int r0 = 4004;
              int r1 = 8008;
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
              int r1 = 8008;
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
            let test_transform : F.Path.Flagged.t Lazy.t -> unit =
              test ~action:(Transform (Fn.const (Ok example_stm)))

            let%expect_test "transform a statement" =
              test_transform P.in_stm_flagged ;
              [%expect
                {|
                  void
                  P0(atomic_int *x, atomic_int *y)
                  {
                      atomic_int r0 = 4004;
                      int r1 = 8008;
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
              int r1 = 8008;
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
          ("Unexpected kind of action associated with this path" (got transform)
           (want insert)) |}]
          end )

        let%test_module "transform_stm_list" =
          ( module struct
            let iffify (statements : F.Subject.Statement.t list) :
                F.Subject.Statement.t list Or_error.t =
              Ok
                [ A.construct Fir.Statement.if_stm
                    (Fir.If.make
                       ~cond:(Fir.Expression.bool_lit true)
                       ~t_branch:
                         (F.Subject.Block.make_generated ~statements ())
                       ~f_branch:(F.Subject.Block.make_generated ())) ]

            let test_transform_list : F.Path.Flagged.t Lazy.t -> unit =
              test ~action:(Transform_list iffify)

            let%expect_test "try to list-transform a statement (invalid)" =
              test_transform_list P.in_stm_flagged ;
              [%expect
                {|
                  ("Unexpected kind of action associated with this path" (got transform_list)
                   (want transform)) |}]

            let%expect_test "list-transform a range" =
              test_transform_list P.surround_atomic ;
              [%expect
                {|
                  void
                  P0(atomic_int *x, atomic_int *y)
                  {
                      atomic_int r0 = 4004;
                      int r1 = 8008;
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
                  ("Unexpected kind of action associated with this path" (got transform_list)
                   (want insert)) |}]

            let test_produce_consume ?(filter : F.Path_filter.t option)
                (action : F.Path_kind.With_action.t) : unit =
              let kind = F.Path_kind.With_action.to_kind action in
              Test.run_exn
                ( module struct
                  type t = F.Path.Flagged.t [@@deriving sexp]

                  let quickcheck_generator =
                    Or_error.ok_exn
                      (F.Path_producers.try_gen_with_flags ?filter ~kind
                         (Lazy.force Subject.Test_data.test))

                  let quickcheck_shrinker =
                    (* for now *)
                    Shrinker.atomic
                end )
                ~f:(fun path ->
                  [%test_result: unit Or_error.t] ~here:[[%here]]
                    ~expect:(Ok ())
                    (Or_error.map ~f:(Fn.const ())
                       (F.Path_consumers.consume_with_flags ~path ?filter
                          ~action
                          (Lazy.force Subject.Test_data.test))))

            let%test_unit "unfiltered transform-stm produces valid paths" =
              test_produce_consume (Transform_list Or_error.return)

            let%test_unit "filter-to-no-rec-labels transform-stm produces \
                           valid paths" =
              test_produce_consume
                ~filter:
                  F.Path_filter.(
                    require_end_check
                      ~check:(Stm_class (Has_not_any, [Prim (Some Label)]))
                      empty)
                (Transform_list Or_error.return)
          end )
      end )
  end )
