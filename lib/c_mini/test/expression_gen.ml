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

let printer (e : Src.Expression.t) : unit =
  e |> Src.Reify.expr |> Fmt.(pr "@[%a@]@." (parens Act_c_lang.Ast.Expr.pp))

let test_all_expressions_have_type
    (f :
         (module Src.Env_types.S_with_known_values)
      -> (module Q.Test.S with type t = Src.Expression.t)) (ty : Src.Type.t)
    : unit =
  let env = Lazy.force Env.det_known_value_mod in
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
         (module Src.Env_types.S_with_known_values)
      -> (module Q.Test.S with type t = Src.Expression.t)) : unit =
  let (module E) = Lazy.force Env.det_known_value_mod in
  let (module Q) = f (module E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:
      ([%test_pred: Src.Expression.t]
         (Src.Expression.On_identifiers.for_all ~f:(Map.mem E.env))
         ~here:[[%here]])

let test_all_expressions_evaluate
    (f :
         (module Src.Env_types.S_with_known_values)
      -> (module Q.Test.S with type t = Src.Expression.t))
    ~(pred : Src.Constant.t -> bool) : unit =
  let env_mod = Lazy.force Env.det_known_value_mod in
  let env = Src.Address.eval_on_env env_mod in
  let (module Qc) = f env_mod in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: bool Or_error.t]
        (let k_result = Src.Expression.Eval.as_constant ~env e in
         Or_error.map ~f:pred k_result)
        ~expect:(Or_error.return true)
        ~equal:[%compare.equal: bool Or_error.t] ~here:[[%here]])

let%test_module "Int_values" =
  ( module struct
    let print_sample (module E : Src.Env_types.S) =
      Qx.print_sample ~printer
        ( module struct
          include Src.Expression
          include Src.Expression_gen.Int_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (-879720314)
      (-186)
      (7627)
      (1234853)
      (57529197)
      (470264907)
      (foo)
      (*blep)
      (atomic_load_explicit(bar, memory_order_seq_cst))
      (atomic_load_explicit(&x, memory_order_acquire))
      (atomic_load_explicit(&x, memory_order_relaxed))
      (atomic_load_explicit(&x, memory_order_consume))
      (atomic_load_explicit(&y, memory_order_acquire)) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (-112015996)
      (-1)
      (1136)
      (7627)
      (13418)
      (33417)
      (10703535)
      (atomic_load_explicit(bar, memory_order_seq_cst))
      (atomic_load_explicit(bar, memory_order_acquire))
      (atomic_load_explicit(bar, memory_order_relaxed))
      (atomic_load_explicit(bar, memory_order_consume)) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (-2147483648)
      (-879720314)
      (-780780327)
      (-50348097)
      (-6117475)
      (-5530953)
      (-4713)
      (-18)
      (-1)
      (31)
      (664)
      (1136)
      (7627)
      (13418)
      (31453)
      (33417)
      (10703535)
      (22551631)
      (71885327)
      (470264907) |}]

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
      Qx.print_sample ~printer
        ( module struct
          include Src.Expression
          include Src.Expression_gen.Bool_values (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env_mod) ;
      [%expect
        {|
      (false)
      (true)
      (barbaz)
      (-879720314 == atomic_load_explicit(&x, memory_order_acquire))
      (-209 == atomic_load_explicit(&x, memory_order_consume))
      (-24 == foo)
      (8 == -98)
      (7471 == 1234853)
      (12062 == 918)
      (foo == foo)
      (atomic_load_explicit(&x, memory_order_acquire) == 57529197)
      (atomic_load_explicit(&x, memory_order_relaxed) == *blep) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (false)
      (true)
      (-32276 == atomic_load_explicit(bar, memory_order_consume))
      (-22537 == -28705)
      (-18 == 664)
      (6 == -32)
      (20 == atomic_load_explicit(bar, memory_order_consume))
      (1129 == atomic_load_explicit(bar, memory_order_seq_cst))
      (1136 == 13418)
      (14202 == -1736309620)
      (18140 == -1)
      (atomic_load_explicit(bar, memory_order_seq_cst) == 10703535)
      (atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(bar, memory_order_acquire)) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (false)
      (true)
      (-38250 == -37287526)
      (-32276 == -23556581)
      (-4713 == -780780327)
      (664 == 7627)
      (1129 == -31235266)
      (7471 == 1234853)
      (509412 == -972508553)
      (57529197 == 115)
      (89301152 == -96) |}]

    let test_fun (module E : Src.Env_types.S_with_known_values) =
      (module Src.Expression_gen.Bool_values (E) : Q.Test.S
        with type t = Src.Expression.t )

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    (* TODO(@MattWindsor91): we can't currently check that all expressions
       evaluate safely to Booleans, as the evaluator and known values
       tracker don't understand atomic loads. *)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          (module Src.Expression_gen.Bool_values ((val e))))
  end )

let%test_module "Bool_tautologies" =
  ( module struct
    let print_sample (module E : Src.Env_types.S_with_known_values) =
      Qx.print_sample ~printer
        ( module struct
          include Src.Expression
          include Src.Expression_gen.Bool_tautologies (E)
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.det_known_value_mod) ;
      [%expect
        {|
          (true)
          (true && atomic_load_explicit(&x, memory_order_relaxed) == *blep || true)
          (false || true)
          (true || barbaz)
          (true || *blep == atomic_load_explicit(bar, memory_order_seq_cst))
          (barbaz || true || -209 == atomic_load_explicit(&x, memory_order_consume))
          (-5 == *blep || -312 == *blep || 1317973 == foo ||
           atomic_load_explicit(&x, memory_order_relaxed) == 1 || true && barbaz ||
           true && true)
          (206117 == atomic_load_explicit(bar, memory_order_relaxed) || true)
          (57529197 == 115 || true)
          (atomic_load_explicit(&y, memory_order_consume) ==
           atomic_load_explicit(&x, memory_order_seq_cst) || true)
          (true || barbaz && -1 == 13585 || false || true && true || foo == -452191315
           && true && true && true && true && true && true && true || true && true ||
           barbaz)
          (true || 470264907 == -879720314 && true ||
           atomic_load_explicit(&x, memory_order_relaxed) == foo || false)
          (true && true && false || 0 == -149 || true || barbaz && true && true || true
           && true || barbaz || barbaz && true && true || barbaz && barbaz ||
           atomic_load_explicit(bar, memory_order_relaxed) == 323 || -3005 == 32743007
           || true && true || false || barbaz || true || barbaz || false || false &&
           true && true || barbaz || 99 == *blep && true && barbaz || true || -427 ==
           atomic_load_explicit(&x, memory_order_acquire) ||
           atomic_load_explicit(&x, memory_order_consume) == -2548 &&
           atomic_load_explicit(&y, memory_order_seq_cst) == -1 || true || barbaz ||
           true && barbaz || true && true || false ||
           atomic_load_explicit(&y, memory_order_consume) == *blep && barbaz || true &&
           true && false || false || true || 1 == foo && -3531150 == -64 || true ||
           *blep == 483016954 || true && true && true || true || -1 ==
           atomic_load_explicit(bar, memory_order_seq_cst) && barbaz || true || barbaz
           || *blep == atomic_load_explicit(bar, memory_order_consume) || true ||
           barbaz || foo == foo && true || barbaz || true && true || false)
          (true || true || barbaz || barbaz || true || barbaz)
          (true || true && barbaz || true || true && true || barbaz && true || true &&
           true || barbaz || true)
          (true && false || false || false || true || barbaz || false || true && true
           && true && barbaz || atomic_load_explicit(&y, memory_order_relaxed) == 800
           || *blep == atomic_load_explicit(&y, memory_order_seq_cst) || true && true
           || true || true || barbaz && true && true || true && foo == *blep || barbaz
           || true && true || true && true || barbaz && barbaz || true || false ||
           false || true || barbaz) |}]

    let test_fun (module E : Src.Env_types.S_with_known_values) =
      (module Src.Expression_gen.Bool_tautologies (E) : Q.Test.S
        with type t = Src.Expression.t )

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    let%test_unit "all expressions evaluate to 'true'" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Src.Constant.as_bool x with Ok b -> b | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          (module Src.Expression_gen.Bool_values ((val e))))
  end )
