(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
open Import

open struct
  module A = Accessor
  module Fir = C4f_fir
end

let%test_module "paths applied to example code" =
  ( module struct
    open struct
      module Stm = Fir.Statement_traverse
      module P = Subject.Test_data.Path
    end

    let test (path : Src.Path.With_meta.t Lazy.t)
        ~(action : Src.Path_kind.With_action.t) : unit =
      let {Src.State.vars; _} = Lazy.force State.Test_data.state in
      Fmt.(
        pr "@[<v>%a@]@."
          (C4f_utils.My_format.pp_or_error
             Action.Test_utils.(using (Fn.flip reify_test vars) pp_tu) )
          (Src.Path_consumers.consume
             (Lazy.force Subject.Test_data.test)
             ~path:(Lazy.force path) ~action ))

    let example_stm : Src.Subject.Statement.t =
      Src.Subject.Statement.make_generated_prim
        Fir.(
          A.(construct (Prim_statement.atomic @> Atomic_statement.store))
            (Atomic_store.make ~mo:Mem_order.Seq_cst
               ~src:(Expression.int_lit 9001)
               ~dst:(Address.of_variable_str_exn "y") ))

    let%test_module "insert_stm" =
      ( module struct
        let test_insert : Src.Path.With_meta.t Lazy.t -> unit =
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
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
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
            for (r1 = 0; r1 <= 2; r1++)
            { atomic_store_explicit(x, 99, memory_order_seq_cst); }
            while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
        }

        void
        P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
        { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "insert onto end of list" =
          test_insert P.insert_end ;
          [%expect
            {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
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
            for (r1 = 0; r1 <= 2; r1++)
            { atomic_store_explicit(x, 99, memory_order_seq_cst); }
            while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
            atomic_store_explicit(y, 9001, memory_order_seq_cst);
        }

        void
        P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
        { loop: ; if (true) {  } else { goto loop; } } |}]
      end )

    let%test_module "transform_stm" =
      ( module struct
        let test_transform : Src.Path.With_meta.t Lazy.t -> unit =
          test ~action:(Transform (Fn.const (Ok example_stm)))

        let%expect_test "transform a statement" =
          test_transform P.in_stm_flagged ;
          [%expect
            {|
                void
                P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
                   bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
                   atomic_int *y)
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
                    for (r1 = 0; r1 <= 2; r1++)
                    { atomic_store_explicit(x, 99, memory_order_seq_cst); }
                    while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
                }

                void
                P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
                   bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
                   atomic_int *y)
                { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "transform a range" =
          test_transform P.surround_atomic ;
          [%expect
            {|
        void
        P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
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
            for (r1 = 0; r1 <= 2; r1++)
            { atomic_store_explicit(x, 99, memory_order_seq_cst); }
            while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
        }

        void
        P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
           bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
           atomic_int *y)
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
        let iffify (statements : Src.Subject.Statement.t list) :
            Src.Subject.Statement.t list Or_error.t =
          Ok
            [ A.construct Fir.Statement.if_stm
                (Fir.If.make
                   ~cond:(Fir.Expression.bool_lit true)
                   ~t_branch:
                     (Src.Subject.Block.make_generated ~statements ())
                   ~f_branch:(Src.Subject.Block.make_generated ()) ) ]

        let test_transform_list : Src.Path.With_meta.t Lazy.t -> unit =
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
                P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
                   bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
                   atomic_int *y)
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
                    for (r1 = 0; r1 <= 2; r1++)
                    { atomic_store_explicit(x, 99, memory_order_seq_cst); }
                    while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
                }

                void
                P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
                   bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
                   atomic_int *y)
                { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "list-transform an insertion (invalid)" =
          test_transform_list P.insert_live ;
          [%expect
            {|
                ("Unexpected kind of action associated with this path" (got transform_list)
                 (want insert)) |}]
      end )
  end )

let%test_module "produce-consume validity" =
  ( module struct
    let test_produce_consume ?(filter : Src.Path_filter.t option)
        (action : Src.Path_kind.With_action.t) : unit =
      let kind = Src.Path_kind.With_action.to_kind action in
      Test.run_exn
        ( module struct
          type t = Src.Path.With_meta.t [@@deriving sexp]

          let quickcheck_generator =
            Or_error.ok_exn
              (Src.Path_producers.try_gen ?filter ~kind
                 (Lazy.force Subject.Test_data.test) )

          let quickcheck_shrinker =
            (* for now *)
            Shrinker.atomic
        end )
        ~f:(fun path ->
          [%test_result: unit Or_error.t] ~here:[[%here]] ~expect:(Ok ())
            (Or_error.map ~f:(Fn.const ())
               (Src.Path_consumers.consume ~path ?filter ~action
                  (Lazy.force Subject.Test_data.test) ) ) )

    let%test_module "transform-list" =
      ( module struct
        let test ?(filter : Src.Path_filter.t option) () : unit =
          test_produce_consume ?filter (Transform_list Or_error.return)

        let%test_unit "unfiltered" = test ()

        let%test_unit "top-anchor" =
          test ~filter:(Src.Path_filter.anchor Top) ()

        let%test_unit "bottom-anchor" =
          test ~filter:(Src.Path_filter.anchor Bottom) ()

        let%test_unit "full-anchor" =
          test ~filter:(Src.Path_filter.anchor Full) ()

        let%test_unit "not any labels" =
          test
            ~filter:
              (Src.Path_filter.require_end_check
                 (Stm_class (Has_not_any, [Prim (Some Label)])) )
            ()
      end )

    let%test_module "transform" =
      ( module struct
        let test ?(filter : Src.Path_filter.t option) () : unit =
          test_produce_consume ?filter (Transform Or_error.return)

        let%test_unit "unfiltered" = test ()

        let%test_unit "top-anchor" =
          test ~filter:(Src.Path_filter.anchor Top) ()

        let%test_unit "bottom-anchor" =
          test ~filter:(Src.Path_filter.anchor Bottom) ()

        let%test_unit "full-anchor" =
          test ~filter:(Src.Path_filter.anchor Full) ()

        let%test_unit "not any labels" =
          test
            ~filter:
              (Src.Path_filter.require_end_check
                 (Stm_class (Has_not_any, [Prim (Some Label)])) )
            ()
      end )

    let%test_module "insert nop" =
      ( module struct
        let test ?(filter : Src.Path_filter.t option) () : unit =
          test_produce_consume ?filter
            (Insert
               [ Src.Subject.Statement.make_generated_prim
                   (Accessor.construct Fir.Prim_statement.nop ()) ] )

        let%test_unit "unfiltered" = test ()

        let%test_unit "top-anchor" =
          test ~filter:(Src.Path_filter.anchor Top) ()

        let%test_unit "bottom-anchor" =
          test ~filter:(Src.Path_filter.anchor Bottom) ()

        let%test_unit "full-anchor" =
          test ~filter:(Src.Path_filter.anchor Full) ()

        let%test_unit "not any labels" =
          test
            ~filter:
              (Src.Path_filter.require_end_check
                 (Stm_class (Has_not_any, [Prim (Some Label)])) )
            ()
      end )
  end )
