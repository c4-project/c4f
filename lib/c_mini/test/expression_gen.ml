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
      (Bool_lit false)
      (Bool_lit true)
      (Lvalue (Variable barbaz))
      (Bop Eq (Constant (Integer -879720314))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire))))
      (Bop Eq (Constant (Integer -209))
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_consume))))
      (Bop Eq (Constant (Integer -24)) (Lvalue (Variable foo)))
      (Bop Eq (Constant (Integer 8)) (Constant (Integer -98)))
      (Bop Eq (Constant (Integer 7471)) (Constant (Integer 1234853)))
      (Bop Eq (Constant (Integer 12062)) (Constant (Integer 918)))
      (Bop Eq (Lvalue (Variable foo)) (Lvalue (Variable foo)))
      (Bop Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_acquire)))
       (Constant (Integer 57529197)))
      (Bop Eq
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_relaxed)))
       (Lvalue (Deref (Variable blep)))) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (Bool_lit false)
      (Bool_lit true)
      (Bop Eq (Constant (Integer -32276))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Bop Eq (Constant (Integer -22537)) (Constant (Integer -28705)))
      (Bop Eq (Constant (Integer -18)) (Constant (Integer 664)))
      (Bop Eq (Constant (Integer 6)) (Constant (Integer -32)))
      (Bop Eq (Constant (Integer 20))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_consume))))
      (Bop Eq (Constant (Integer 1129))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst))))
      (Bop Eq (Constant (Integer 1136)) (Constant (Integer 13418)))
      (Bop Eq (Constant (Integer 14202)) (Constant (Integer -1736309620)))
      (Bop Eq (Constant (Integer 18140)) (Constant (Integer -1)))
      (Bop Eq
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Constant (Integer 10703535)))
      (Bop Eq
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_seq_cst)))
       (Atomic_load ((src (Lvalue (Variable bar))) (mo memory_order_acquire)))) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (Bool_lit false)
      (Bool_lit true)
      (Bop Eq (Constant (Integer -38250)) (Constant (Integer -37287526)))
      (Bop Eq (Constant (Integer -32276)) (Constant (Integer -23556581)))
      (Bop Eq (Constant (Integer -4713)) (Constant (Integer -780780327)))
      (Bop Eq (Constant (Integer 664)) (Constant (Integer 7627)))
      (Bop Eq (Constant (Integer 1129)) (Constant (Integer -31235266)))
      (Bop Eq (Constant (Integer 7471)) (Constant (Integer 1234853)))
      (Bop Eq (Constant (Integer 509412)) (Constant (Integer -972508553)))
      (Bop Eq (Constant (Integer 57529197)) (Constant (Integer 115)))
      (Bop Eq (Constant (Integer 89301152)) (Constant (Integer -96))) |}]

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type
        (fun e -> (module Src.Expression_gen.Bool_values ((val e))))
        Src.Type.(bool ())

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          (module Src.Expression_gen.Bool_values ((val e))))
  end )
