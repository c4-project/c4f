(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_c_mini
open Expression_gen
module Q = Base_quickcheck

let print_sample = Act_utils.My_quickcheck.print_sample

let test_all_expressions_have_type
    (f :
         (module Env_types.S)
      -> (module Base_quickcheck.Test.S with type t = Expression.t))
    (ty : Type.t) : unit =
  let env = Lazy.force Env.test_env_mod in
  let (module Q) = f env in
  let module Ty = Expression.Type_check ((val env)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:(fun e ->
      [%test_result: Type.t Or_error.t] (Ty.type_of e) ~here:[[%here]]
        ~equal:[%compare.equal: Type.t Or_error.t]
        ~expect:(Or_error.return ty))

let test_all_expressions_in_env
    (f :
         (module Env_types.S)
      -> (module Base_quickcheck.Test.S with type t = Expression.t)) : unit
    =
  let (module E) = Lazy.force Env.test_env_mod in
  let (module Q) = f (module E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:
      ([%test_pred: Expression.t]
         (Expression.On_identifiers.for_all ~f:(Map.mem E.env))
         ~here:[[%here]])

let%test_module "Int_values" =
  ( module struct
    let print_sample (module E : Act_c_mini.Env_types.S) =
      print_sample
        ( module struct
          include Act_c_mini.Expression
          include Int_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (Constant (Integer -879720314))
      (Constant (Integer -186))
      (Constant (Integer 7627))
      (Constant (Integer 1234853))
      (Constant (Integer 57529197))
      (Constant (Integer 470264907))
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
      (Constant (Integer -112015996))
      (Constant (Integer -1))
      (Constant (Integer 1136))
      (Constant (Integer 7627))
      (Constant (Integer 13418))
      (Constant (Integer 33417))
      (Constant (Integer 10703535))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_acquire)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_relaxed)))
      (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (Constant (Integer -2147483648))
      (Constant (Integer -879720314))
      (Constant (Integer -780780327))
      (Constant (Integer -50348097))
      (Constant (Integer -6117475))
      (Constant (Integer -5530953))
      (Constant (Integer -4713))
      (Constant (Integer -18))
      (Constant (Integer -1))
      (Constant (Integer 31))
      (Constant (Integer 664))
      (Constant (Integer 1136))
      (Constant (Integer 7627))
      (Constant (Integer 13418))
      (Constant (Integer 31453))
      (Constant (Integer 33417))
      (Constant (Integer 10703535))
      (Constant (Integer 22551631))
      (Constant (Integer 71885327))
      (Constant (Integer 470264907)) |}]

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type
        (fun e -> (module Int_values ((val e))))
        Type.(normal Basic.int)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e -> (module Int_values ((val e))))
  end )

let%test_module "Bool_values" =
  ( module struct
    let print_sample (module E : Act_c_mini.Env_types.S) =
      print_sample
        ( module struct
          include Act_c_mini.Expression
          include Bool_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (Bool_lit false)
      (Bool_lit true)
      (Lvalue (Variable barbaz))
      (Eq (Constant (Integer -879720314))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire))))
      (Eq (Constant (Integer -209))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_consume))))
      (Eq (Constant (Integer -24)) (Lvalue (Variable foo)))
      (Eq (Constant (Integer 8)) (Constant (Integer -98)))
      (Eq (Constant (Integer 7471)) (Constant (Integer 1234853)))
      (Eq (Constant (Integer 12062)) (Constant (Integer 918)))
      (Eq (Lvalue (Variable foo)) (Lvalue (Variable foo)))
      (Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire)))
       (Constant (Integer 57529197)))
      (Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_relaxed)))
       (Lvalue (Deref (Variable blep)))) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (Bool_lit false)
      (Bool_lit true)
      (Eq (Constant (Integer -32276))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Eq (Constant (Integer -22537)) (Constant (Integer -28705)))
      (Eq (Constant (Integer -18)) (Constant (Integer 664)))
      (Eq (Constant (Integer 6)) (Constant (Integer -32)))
      (Eq (Constant (Integer 20))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Eq (Constant (Integer 1129))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst))))
      (Eq (Constant (Integer 1136)) (Constant (Integer 13418)))
      (Eq (Constant (Integer 14202)) (Constant (Integer -1736309620)))
      (Eq (Constant (Integer 18140)) (Constant (Integer -1)))
      (Eq (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Constant (Integer 10703535)))
      (Eq (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_acquire)))) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (Bool_lit false)
      (Bool_lit true)
      (Eq (Constant (Integer -38250)) (Constant (Integer -37287526)))
      (Eq (Constant (Integer -32276)) (Constant (Integer -23556581)))
      (Eq (Constant (Integer -4713)) (Constant (Integer -780780327)))
      (Eq (Constant (Integer 664)) (Constant (Integer 7627)))
      (Eq (Constant (Integer 1129)) (Constant (Integer -31235266)))
      (Eq (Constant (Integer 7471)) (Constant (Integer 1234853)))
      (Eq (Constant (Integer 509412)) (Constant (Integer -972508553)))
      (Eq (Constant (Integer 57529197)) (Constant (Integer 115)))
      (Eq (Constant (Integer 89301152)) (Constant (Integer -96))) |}]

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type
        (fun e -> (module Bool_values ((val e))))
        Type.(normal Basic.bool)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e -> (module Bool_values ((val e))))
  end )
