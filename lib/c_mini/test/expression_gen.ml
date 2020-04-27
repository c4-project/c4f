(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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
  e |> Src.Reify_expr.reify |> Fmt.(pr "@[%a@]@." Act_c_lang.Ast.Expr.pp)

let print_sample (module G : Src.Expression_gen.S) : unit =
  (* Expressions are quite big, so we tone down the generation parameters a
     bit. *)
  Qx.print_sample ~test_count:5 ~printer
    ( module struct
      include Src.Expression
      include G
    end )

let test_all_expressions_have_type
    (f : Src.Env.t -> (module Q.Test.S with type t = Src.Expression.t))
    (ty : Src.Type.t) : unit =
  let env = Lazy.force Env.test_env in
  let (module Qc) = f env in
  let module Ty = Src.Expression.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: Src.Type.t Or_error.t] (Ty.type_of e) ~here:[[%here]]
        ~equal:[%compare.equal: Src.Type.t Or_error.t]
        ~expect:(Or_error.return ty))

module Exp_idents =
  Travesty.Traversable.Chain0
    (Act_c_mini.Expression_traverse.On_lvalues)
    (Act_c_mini.Lvalue.On_identifiers)

let test_all_expressions_in_env
    (f : Src.Env.t -> (module Q.Test.S with type t = Src.Expression.t)) :
    unit =
  let env = Lazy.force Env.test_env in
  Base_quickcheck.Test.run_exn (f env)
    ~f:
      ([%test_pred: Src.Expression.t]
         (Exp_idents.for_all ~f:(Map.mem env))
         ~here:[[%here]])

let test_all_expressions_evaluate
    (f : Src.Env.t -> (module Q.Test.S with type t = Src.Expression.t))
    ~(pred : Src.Constant.t -> bool) : unit =
  let env = Lazy.force Env.test_env in
  let heap = Src.Heap.make (Src.Address.eval_on_env ~env) in
  let (module Qc) = f env in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: bool Or_error.t]
        (let k_result = Src.Expression_eval.as_constant ~env:heap e in
         Or_error.map ~f:pred k_result)
        ~expect:(Or_error.return true)
        ~equal:[%compare.equal: bool Or_error.t] ~here:[[%here]])

let%test_module "Int_values" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      print_sample
        ( module Src.Expression_gen.Int_values (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
      -22537
      0
      atomic_fetch_sub_explicit(&y, 0, memory_order_seq_cst)
      foo - 4
      *blep - 99 |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0
        atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst)
        atomic_fetch_sub_explicit(bar,
                                  atomic_fetch_add_explicit(bar, 0,
                                                            memory_order_relaxed)
                                  -
                                  atomic_fetch_add_explicit(bar, 0,
                                                            memory_order_relaxed),
                                  memory_order_acq_rel)
        atomic_load_explicit(bar, memory_order_relaxed)
        atomic_fetch_add_explicit(bar, 0, memory_order_relaxed) - 95 |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        -23556581
        -38250
        0
        16
        3 - 3 |}]

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type
        (fun e ->
          ( module Src.Expression_gen.Int_values (struct
            let env = e
          end) ))
        Src.Type.(int ())

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expression_gen.Int_values (struct
            let env = e
          end) ))
  end )

let%test_module "Int zeroes" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      print_sample
        ( module Src.Expression_gen.Int_zeroes (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
          0
          -22537 - -22537
          foo - 4
          atomic_fetch_sub_explicit(&y, atomic_load_explicit(&y, memory_order_seq_cst)
                                    - 53, memory_order_relaxed)
          -
          atomic_fetch_sub_explicit(&y, atomic_load_explicit(&y, memory_order_seq_cst)
                                    - 53, memory_order_relaxed)
          atomic_load_explicit(&x, memory_order_seq_cst) -
          atomic_load_explicit(&x, memory_order_seq_cst) |}]

    let test_fun (env : Src.Env.t) :
        (module Q.Test.S with type t = Src.Expression.t) =
      ( module Src.Expression_gen.Int_zeroes (struct
        let env = env
      end) )

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type test_fun Src.Type.(int ())

    let%test_unit "all expressions evaluate to 0" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Src.Constant.as_int x with Ok 0 -> true | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expression_gen.Int_zeroes (struct
            let env = e
          end) ))
  end )

let%test_module "Bool_values" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      print_sample
        ( module Src.Expression_gen.Bool_values (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
      false
      -190264 == 0
      (0 == atomic_load_explicit(&x, memory_order_acquire) &&
       atomic_load_explicit(&y, memory_order_consume) == 0 || barbaz)
      && true
      -112015996 == 22551631 || barbaz
      !(1129 == *blep) || !barbaz |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0 == 0
        atomic_fetch_sub_explicit(bar, 0, memory_order_seq_cst) == 0
        !(0 == 0) && (0 == -879720314 && 0 == 7471)
        true || atomic_load_explicit(bar, memory_order_consume) == 0
        atomic_load_explicit(bar, memory_order_consume) ==
        atomic_load_explicit(bar, memory_order_relaxed) - 95 &&
        atomic_load_explicit(bar, memory_order_acquire) == 6126 ||
        atomic_load_explicit(bar, memory_order_relaxed) == 40877287 - 40877287 &&
        (0 == atomic_fetch_add_explicit(bar, 0, memory_order_relaxed) && false) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        -190264 == 0
        0 == 0
        !(0 == 0) && (0 == -879720314 && 0 == 7471)
        true || -22537 == 0
        -4 == 7627 - 7627 && false || (0 == 115 || false || 0 == -4713 && 33209 == 0) |}]

    let test_fun (env : Src.Env.t) =
      ( module Src.Expression_gen.Bool_values (struct
        let env = env
      end) : Q.Test.S
        with type t = Src.Expression.t )

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    (* TODO(@MattWindsor91): we can't currently check that all expressions
       evaluate safely to Booleans, as the evaluator and known values tracker
       don't understand atomic loads. *)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expression_gen.Bool_values (struct
            let env = e
          end) ))
  end )

let%test_module "Bool falsehoods" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      let module B = Src.Expression_gen.Bool_known (struct
        let env = env
      end) in
      print_sample (module B.Falsehoods)

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
          false
          false && barbaz
          (barbaz || 0 == 0) && barbaz && (barbaz && (false || false))
          barbaz && !true && barbaz || !true |}]

    let test_fun (env : Src.Env.t) :
        (module Q.Test.S with type t = Src.Expression.t) =
      let module K = Src.Expression_gen.Bool_known (struct
        let env = env
      end) in
      (module K.Falsehoods)

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    let%test_unit "all expressions evaluate to 'false'" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Src.Constant.as_bool x with Ok b -> not b | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expression_gen.Bool_values (struct
            let env = e
          end) ))
  end )

let%test_module "Bool tautologies" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      let module B = Src.Expression_gen.Bool_known (struct
        let env = env
      end) in
      print_sample (module B.Tautologies)

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
          true
          atomic_load_explicit(&z, memory_order_seq_cst) == false
          true || barbaz
          (barbaz && 0 == atomic_load_explicit(&x, memory_order_acquire) ||
           atomic_load_explicit(&y, memory_order_consume) == 0)
          && barbaz || true
          -112015996 == 22551631 || -153 == foo && true || true |}]

    let test_fun (env : Src.Env.t) :
        (module Q.Test.S with type t = Src.Expression.t) =
      let module K = Src.Expression_gen.Bool_known (struct
        let env = env
      end) in
      (module K.Tautologies)

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    let%test_unit "all expressions evaluate to 'true'" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Src.Constant.as_bool x with Ok b -> b | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expression_gen.Bool_values (struct
            let env = e
          end) ))
  end )
