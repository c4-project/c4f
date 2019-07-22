(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_c_mini
module Q = Base_quickcheck
module Qx = Act_utils.My_quickcheck

let test_all_expressions_have_type
    (f :
         (module Src.Env_types.S)
      -> (module Q.Test.S with type t = Src.Expression.t)) (ty : Src.Type.t)
    : unit =
  let env = Lazy.force Env.test_env_mod in
  let (module Qc) = f env in
  let module Ty = Src.Expression.Type_check ((val env)) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: Src.Type.t Or_error.t] (Ty.type_of e) ~here:[[%here]]
        ~equal:[%compare.equal: Src.Type.t Or_error.t]
        ~expect:(Or_error.return ty))

let test_all_expressions_in_env
    (f :
         (module Src.Env_types.S)
      -> (module Q.Test.S with type t = Src.Expression.t)) : unit =
  let (module E) = Lazy.force Env.test_env_mod in
  let (module Q) = f (module E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:
      ([%test_pred: Src.Expression.t]
         (Src.Expression.On_identifiers.for_all ~f:(Map.mem E.env))
         ~here:[[%here]])

let%test_module "Int_values" =
  ( module struct
    let print_sample (module E : Src.Env_types.S) =
      Qx.print_sample
        ( module struct
          include Src.Expression
          include Src.Expression_gen.Int_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (Constant (Int -879720314))
      (Constant (Int -186))
      (Constant (Int 7627))
      (Constant (Int 1234853))
      (Constant (Int 57529197))
      (Constant (Int 470264907))
      (Lvalue (Variable foo))
      (Lvalue (Deref (Variable blep)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
      (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire)))
      (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_relaxed)))
      (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_consume)))
      (Atomic_load ((src (Ref (Lvalue (Variable y)))) (mo memory_order_acquire))) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (Constant (Int -112015996))
      (Constant (Int -1))
      (Constant (Int 1136))
      (Constant (Int 7627))
      (Constant (Int 13418))
      (Constant (Int 33417))
      (Constant (Int 10703535))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_acquire)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_relaxed)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (Constant (Int -2147483648))
      (Constant (Int -879720314))
      (Constant (Int -780780327))
      (Constant (Int -50348097))
      (Constant (Int -6117475))
      (Constant (Int -5530953))
      (Constant (Int -4713))
      (Constant (Int -18))
      (Constant (Int -1))
      (Constant (Int 31))
      (Constant (Int 664))
      (Constant (Int 1136))
      (Constant (Int 7627))
      (Constant (Int 13418))
      (Constant (Int 31453))
      (Constant (Int 33417))
      (Constant (Int 10703535))
      (Constant (Int 22551631))
      (Constant (Int 71885327))
      (Constant (Int 470264907)) |}]

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type
        (fun e -> (module Src.Expression_gen.Int_values ((val e))))
        Src.Type.(int ())

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          (module Src.Expression_gen.Int_values ((val e))))
  end )

let%test_module "Bool_values" =
  ( module struct
    let print_sample (module E : Src.Env_types.S) =
      Qx.print_sample
        ( module struct
          include Src.Expression
          include Src.Expression_gen.Bool_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (Constant (Bool false))
      (Constant (Bool true))
      (Lvalue (Variable barbaz))
      (Bop Eq (Constant (Int -879720314))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire))))
      (Bop Eq (Constant (Int -209))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_consume))))
      (Bop Eq (Constant (Int -24)) (Lvalue (Variable foo)))
      (Bop Eq (Constant (Int 8)) (Constant (Int -98)))
      (Bop Eq (Constant (Int 7471)) (Constant (Int 1234853)))
      (Bop Eq (Constant (Int 12062)) (Constant (Int 918)))
      (Bop Eq (Lvalue (Variable foo)) (Lvalue (Variable foo)))
      (Bop Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire)))
       (Constant (Int 57529197)))
      (Bop Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_relaxed)))
       (Lvalue (Deref (Variable blep)))) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (Constant (Bool false))
      (Constant (Bool true))
      (Bop Eq (Constant (Int -32276))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Bop Eq (Constant (Int -22537)) (Constant (Int -28705)))
      (Bop Eq (Constant (Int -18)) (Constant (Int 664)))
      (Bop Eq (Constant (Int 6)) (Constant (Int -32)))
      (Bop Eq (Constant (Int 20))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Bop Eq (Constant (Int 1129))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst))))
      (Bop Eq (Constant (Int 1136)) (Constant (Int 13418)))
      (Bop Eq (Constant (Int 14202)) (Constant (Int -1736309620)))
      (Bop Eq (Constant (Int 18140)) (Constant (Int -1)))
      (Bop Eq
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Constant (Int 10703535)))
      (Bop Eq
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_acquire)))) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (Constant (Bool false))
      (Constant (Bool true))
      (Bop Eq (Constant (Int -38250)) (Constant (Int -37287526)))
      (Bop Eq (Constant (Int -32276)) (Constant (Int -23556581)))
      (Bop Eq (Constant (Int -4713)) (Constant (Int -780780327)))
      (Bop Eq (Constant (Int 664)) (Constant (Int 7627)))
      (Bop Eq (Constant (Int 1129)) (Constant (Int -31235266)))
      (Bop Eq (Constant (Int 7471)) (Constant (Int 1234853)))
      (Bop Eq (Constant (Int 509412)) (Constant (Int -972508553)))
      (Bop Eq (Constant (Int 57529197)) (Constant (Int 115)))
      (Bop Eq (Constant (Int 89301152)) (Constant (Int -96))) |}]

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type
        (fun e -> (module Src.Expression_gen.Bool_values ((val e))))
        Src.Type.(bool ())

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          (module Src.Expression_gen.Bool_values ((val e))))
  end )
