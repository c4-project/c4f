(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = Act_fir
  module Src = Act_litmus_c
end

let%test_module "examples" =
  ( module struct
    let test : Fir.Expression.t -> unit = Fmt.pr "@[%a@]@." Src.Reify_expr.pp

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
               ~src:(Address.of_variable_ref (Act_common.C_id.of_string "x"))
               ~mo:Mem_order.Seq_cst)) ;
      [%expect {| atomic_load_explicit(&x, memory_order_seq_cst) |}]
  end )

let%test_module "round trips" =
  ( module struct
    let test_round_trip
        (module Qc : Act_utils.My_quickcheck.S_with_sexp
          with type t = Fir.Expression.t) : unit =
      Base_quickcheck.Test.run_exn
        (module Qc)
        ~f:(fun exp ->
          [%test_result: Fir.Expression.t Or_error.t] ~here:[[%here]]
            ~expect:(Ok exp)
            (Src.Abstract_expr.model (Src.Reify_expr.reify exp)))

    module Make (F : functor (A : Fir.Env_types.S) ->
      Act_utils.My_quickcheck.S_with_sexp with type t = Fir.Expression.t) =
    struct
      let run_round_trip () : unit =
        let env = Lazy.force Act_fir_test.Env.test_env in
        let module Qc = F (struct
          let env = env
        end) in
        test_round_trip (module Qc)
    end

    module Int_values = Make (Fir.Expression_gen.Int_values)

    let%test_unit "round-trip on integer expressions" =
      Int_values.run_round_trip ()

    module Int_zeroes = Make (Fir.Expression_gen.Int_zeroes)

    let%test_unit "round-trip on integer zeroes" =
      Int_zeroes.run_round_trip ()

    module Bool_values = Make (Fir.Expression_gen.Bool_values)

    let%test_unit "round-trip on Boolean expressions" =
      Bool_values.run_round_trip ()

    module Bool_tautologies_in (E : Fir.Env_types.S) = struct
      module K = Fir.Expression_gen.Bool_known (E)
      include K.Tautologies
    end

    module Bool_tautologies = Make (Bool_tautologies_in)

    let%test_unit "round-trip on Boolean tautologies" =
      Bool_tautologies.run_round_trip ()

    module Bool_falsehoods_in (E : Fir.Env_types.S) = struct
      module K = Fir.Expression_gen.Bool_known (E)
      include K.Tautologies
    end

    module Bool_falsehoods = Make (Bool_falsehoods_in)

    let%test_unit "round-trip on Boolean falsehoods" =
      Bool_falsehoods.run_round_trip ()
  end )