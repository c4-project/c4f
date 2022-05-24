(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = C4f_fir
  module Fir_gen = C4f_fir_gen
  module Src = C4f_litmus_c
end

let%test_module "examples" =
  ( module struct
    let test : Fir.Expression.t -> unit = Fmt.pr "@[%a@]@." Src.Reify_expr.pp

    let%expect_test "nested ternary, right" =
      test
        Fir.Expression.(
          ternary
            { if_= of_variable_str_exn "a"
            ; then_= int_lit 1
            ; else_=
                ternary
                  { if_= of_variable_str_exn "b"
                  ; then_= int_lit 2
                  ; else_= int_lit 3 } }) ;
      [%expect {| a ? 1 : b ? 2 : 3 |}]

    let%expect_test "nested ternary, middle" =
      test
        Fir.Expression.(
          ternary
            { if_= of_variable_str_exn "a"
            ; then_=
                ternary
                  { if_= of_variable_str_exn "b"
                  ; then_= int_lit 1
                  ; else_= int_lit 2 }
            ; else_= int_lit 3 }) ;
      [%expect {| a ? b ? 1 : 2 : 3 |}]

    let%expect_test "nested ternary, left" =
      test
        Fir.Expression.(
          ternary
            { if_=
                ternary
                  { if_= of_variable_str_exn "a"
                  ; then_= int_lit 1
                  ; else_= int_lit 2 }
            ; then_= of_variable_str_exn "b"
            ; else_= int_lit 3 }) ;
      [%expect {| (a ? 1 : 2) ? b : 3 |}]

    let%expect_test "subtract bracketing 1" =
      test
        Fir.Expression.(
          sub (sub (int_lit 1) (int_lit 1)) (sub (int_lit 1) (int_lit 1))) ;
      [%expect {| 1 - 1 - (1 - 1) |}]

    let%expect_test "subtract bracketing 2" =
      test
        Fir.Expression.(
          sub (sub (sub (int_lit 1) (int_lit 1)) (int_lit 1)) (int_lit 1)) ;
      [%expect {| 1 - 1 - 1 - 1 |}]

    let%expect_test "subtract bracketing 3" =
      test
        Fir.Expression.(
          sub (int_lit 1) (sub (int_lit 1) (sub (int_lit 1) (int_lit 1)))) ;
      [%expect {| 1 - (1 - (1 - 1)) |}]

    let%expect_test "add-subtract bracketing" =
      test
        Fir.Expression.(
          sub (add (sub (int_lit 1) (int_lit 2)) (int_lit 3)) (int_lit 4)) ;
      [%expect {| 1 - 2 + 3 - 4 |}]

    let%expect_test "atomic_load of referenced variable" =
      test
        Fir.(
          Expression.atomic_load
            (Atomic_load.make
               ~src:
                 (Accessor.construct Address.variable_ref
                    (C4f_common.C_id.of_string "x") )
               ~mo:Mem_order.Seq_cst )) ;
      [%expect {| atomic_load_explicit(&x, memory_order_seq_cst) |}]
  end )

let%test_module "round trips" =
  ( module struct
    let test_round_trip
        (module Qc : C4f_utils.My_quickcheck.S_with_sexp
          with type t = Fir.Expression.t ) : unit =
      Base_quickcheck.Test.run_exn
        (module Qc)
        ~f:(fun exp ->
          [%test_result: Fir.Expression.t Or_error.t] ~here:[[%here]]
            ~expect:(Ok exp)
            (Src.Abstract_expr.model (Src.Reify_expr.reify exp)) )

    module Make (F : functor (A : Fir.Env_types.S) ->
      C4f_utils.My_quickcheck.S_with_sexp with type t = Fir.Expression.t) =
    struct
      let run_round_trip () : unit =
        let env = Lazy.force C4f_fir_test.Env.test_env in
        let module Qc = F (struct
          let env = env
        end) in
        test_round_trip (module Qc)
    end

    module Int_values = Make (Fir_gen.Expr.Int_values)

    let%test_unit "round-trip on integer expressions" =
      Int_values.run_round_trip ()

    module Int_zeroes = Make (Fir_gen.Expr.Int_zeroes)

    let%test_unit "round-trip on integer zeroes" =
      Int_zeroes.run_round_trip ()

    module Bool_values = Make (Fir_gen.Expr.Bool_values)

    let%test_unit "round-trip on Boolean expressions" =
      Bool_values.run_round_trip ()

    module Bool_tautologies_in (E : Fir.Env_types.S) = struct
      module K = Fir_gen.Expr.Bool_known (E)
      include K.Tautologies
    end

    module Bool_tautologies = Make (Bool_tautologies_in)

    let%test_unit "round-trip on Boolean tautologies" =
      Bool_tautologies.run_round_trip ()

    module Bool_falsehoods_in (E : Fir.Env_types.S) = struct
      module K = Fir_gen.Expr.Bool_known (E)
      include K.Tautologies
    end

    module Bool_falsehoods = Make (Bool_falsehoods_in)

    let%test_unit "round-trip on Boolean falsehoods" =
      Bool_falsehoods.run_round_trip ()
  end )
