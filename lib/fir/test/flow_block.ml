(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_fir

let%test_module "On_lvalues" =
  ( module struct
    module FHList = Src.Flow_block.Header.On_lvalues.On_monad (List)
    module F = Src.Statement_traverse.Flow_block.With_meta (Unit)
    module FList = F.On_lvalues.On_monad (List)
    module S = Src.Statement_traverse.With_meta (Unit)
    module SList = S.On_lvalues.On_monad (List)

    let header : Src.Flow_block.Header.t =
      Src.(
        For
          { init=
              Some
                Assign.(
                  Lvalue.of_variable_str_exn "x" @= Expression.int_lit 0)
          ; cmp=
              Some Expression.(Infix.(of_variable_str_exn "x" < int_lit 5))
          ; update=
              Some
                (Assign.make ~dst:(Lvalue.of_variable_str_exn "x") ~src:Inc)
          })

    let fb =
      Src.Flow_block.make ~header ~body:(Src.Block.make ~metadata:() ())

    let%expect_test "for-loop header" =
      let headers = FHList.map_m header ~f:(fun lv -> [lv; Deref lv]) in
      Stdio.print_s [%sexp (headers : Src.Flow_block.Header.t list)] ;
      [%expect
        {|
        ((For
          ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
           (cmp ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
           (update (((dst (Variable x)) (src Inc))))))
         (For
          ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
           (cmp ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
           (update (((dst (Deref (Variable x))) (src Inc))))))
         (For
          ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
           (cmp
            ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
              (Constant (Int 5)))))
           (update (((dst (Variable x)) (src Inc))))))
         (For
          ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
           (cmp
            ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
              (Constant (Int 5)))))
           (update (((dst (Deref (Variable x))) (src Inc))))))
         (For
          ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
           (cmp ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
           (update (((dst (Variable x)) (src Inc))))))
         (For
          ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
           (cmp ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
           (update (((dst (Deref (Variable x))) (src Inc))))))
         (For
          ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
           (cmp
            ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
              (Constant (Int 5)))))
           (update (((dst (Variable x)) (src Inc))))))
         (For
          ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
           (cmp
            ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
              (Constant (Int 5)))))
           (update (((dst (Deref (Variable x))) (src Inc))))))) |}]

    let%expect_test "for-loop" =
      let fbs = FList.map_m fb ~f:(fun lv -> [lv; Deref lv]) in
      Stdio.print_s [%sexp (fbs : (unit, _) Src.Flow_block.t list)] ;
      [%expect
        {|
        (((header
           (For
            ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
             (update (((dst (Variable x)) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
             (update (((dst (Deref (Variable x))) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                (Constant (Int 5)))))
             (update (((dst (Variable x)) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                (Constant (Int 5)))))
             (update (((dst (Deref (Variable x))) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
             (update (((dst (Variable x)) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
             (update (((dst (Deref (Variable x))) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                (Constant (Int 5)))))
             (update (((dst (Variable x)) (src Inc)))))))
          (body ((statements ()) (metadata ()))))
         ((header
           (For
            ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
             (cmp
              ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                (Constant (Int 5)))))
             (update (((dst (Deref (Variable x))) (src Inc)))))))
          (body ((statements ()) (metadata ()))))) |}]

    let%expect_test "for-loop in an expression" =
      let stm = Accessor.construct Src.Statement.flow fb in
      let stms = SList.map_m stm ~f:(fun lv -> [lv; Deref lv]) in
      Stdio.print_s [%sexp (stms : unit Src.Statement.t list)] ;
      [%expect
        {|
        ((Flow
          ((header
            (For
             ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
              (update (((dst (Variable x)) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
              (update (((dst (Deref (Variable x))) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                 (Constant (Int 5)))))
              (update (((dst (Variable x)) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Variable x)) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                 (Constant (Int 5)))))
              (update (((dst (Deref (Variable x))) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
              (update (((dst (Variable x)) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Variable x))) (Constant (Int 5)))))
              (update (((dst (Deref (Variable x))) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                 (Constant (Int 5)))))
              (update (((dst (Variable x)) (src Inc)))))))
           (body ((statements ()) (metadata ())))))
         (Flow
          ((header
            (For
             ((init (((dst (Deref (Variable x))) (src (Expr (Constant (Int 0)))))))
              (cmp
               ((Bop (Rel Lt) (Address (Lvalue (Deref (Variable x))))
                 (Constant (Int 5)))))
              (update (((dst (Deref (Variable x))) (src Inc)))))))
           (body ((statements ()) (metadata ())))))) |}]
  end )

let%test_module "for loop simplification" =
  ( module struct
    let%test_module "try_simplify" =
      ( module struct
        let test (l : Act_fir.Flow_block.For.t) : unit =
          Stdio.print_s
            [%sexp
              ( Act_fir.Flow_block.For.try_simplify l
                : Act_fir.Flow_block.For.Simple.t Or_error.t )]

        let%expect_test "upwards loop" =
          test
            Act_fir.(
              Flow_block.For.make
                ~init:
                  Assign.(
                    Lvalue.of_variable_str_exn "x" @= Expression.int_lit 0)
                ~cmp:
                  Expression.(
                    Infix.(
                      Expression.of_variable_str_exn "x"
                      < Expression.int_lit 42))
                ~update:
                  (Assign.make
                     ~dst:(Lvalue.of_variable_str_exn "x")
                     ~src:Inc)
                ()) ;
          [%expect
            {|
      (Ok
       ((lvalue (Variable x)) (init_value (Constant (Int 0)))
        (cmp_value (Constant (Int 42))) (direction (Up Exclusive)))) |}]
      end )

    let%test_unit "simple first variant law" =
      Base_quickcheck.Test.run_exn
        ( module struct
          type t = Act_fir.Flow_block.For.Simple.t [@@deriving sexp]

          let quickcheck_generator : t Base_quickcheck.Generator.t =
            Base_quickcheck.Generator.(
              return (fun lvalue init_value cmp_value direction ->
                  { Act_fir.Flow_block.For.Simple.lvalue
                  ; init_value
                  ; cmp_value
                  ; direction })
              <*> Act_fir.Lvalue.quickcheck_generator
              <*> map ~f:Act_fir.Expression.int_lit
                    small_positive_or_zero_int
              <*> map ~f:Act_fir.Expression.int_lit
                    small_strictly_positive_int
              <*> Act_fir.Flow_block.For.Simple.Direction
                  .quickcheck_generator)

          let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
            Base_quickcheck.Shrinker.atomic
        end )
        ~f:(fun t ->
          [%test_result: Act_fir.Flow_block.For.Simple.t option]
            ~here:[[%here]] ~expect:(Some t)
            Accessor.(
              Act_fir.Flow_block.For.((construct simple t).@?(simple))))
  end )
