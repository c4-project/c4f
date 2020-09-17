(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

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
        (cmp_value (Constant (Int 42))) (direction Up_exclusive))) |}]
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
