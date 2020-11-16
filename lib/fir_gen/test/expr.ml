(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let printer : Fir.Expression.t -> unit =
  Fmt.(pr "@[%a@]@." Act_litmus_c.Reify_expr.pp)

let print_sample (module G : Src.Expr.S) : unit =
  (* Expressions are quite big, so we tone down the generation parameters a
     bit. *)
  Utils.My_quickcheck.print_sample ~test_count:6 ~printer
    ( module struct
      include Fir.Expression
      include G
    end )

let test_all_expressions_have_type
    (f : Fir.Env.t -> (module Q.Test.S with type t = Fir.Expression.t))
    (ty : Fir.Type.t) : unit =
  let env = Lazy.force Fir_test.Env.test_env in
  let (module Qc) = f env in
  let module Ty = Fir.Expression.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: Fir.Type.t Or_error.t] (Ty.type_of e) ~here:[[%here]]
        ~equal:[%compare.equal: Fir.Type.t Or_error.t]
        ~expect:(Or_error.return ty))

let test_all_expressions_in_env
    (f : Fir.Env.t -> (module Q.Test.S with type t = Fir.Expression.t)) :
    unit =
  let env = Lazy.force Fir_test.Env.test_env in
  Base_quickcheck.Test.run_exn (f env)
    ~f:
      ([%test_pred: Fir.Expression.t]
         (Fir.Expression_traverse.On_addresses.for_all
            ~f:(Accessor.for_all Fir.Address.variable_of ~f:(Map.mem env)))
         ~here:[[%here]])

let test_all_expressions_evaluate
    (f : Fir.Env.t -> (module Q.Test.S with type t = Fir.Expression.t))
    ~(pred : Fir.Constant.t -> bool) : unit =
  let env = Lazy.force Fir_test.Env.test_env in
  let heap = Fir.Heap.make (Fir.Address.eval_on_env ~env) in
  let (module Qc) = f env in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      [%test_result: bool Or_error.t]
        (let k_result = Fir.Expression_eval.as_constant ~env:heap e in
         Or_error.map ~f:pred k_result)
        ~expect:(Or_error.return true)
        ~equal:[%compare.equal: bool Or_error.t] ~here:[[%here]])

let%test_module "Int_values" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      print_sample
        ( module Src.Expr.Int_values (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
      0
      foo
      95 ^ atomic_load_explicit(bar, memory_order_consume)
      *blep ^ foo |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Fir_test.Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0
        atomic_load_explicit(bar, memory_order_consume)
        atomic_load_explicit(bar, memory_order_acquire)
        atomic_load_explicit(bar, memory_order_consume) | 0 &
        atomic_load_explicit(bar, memory_order_acquire)
        95 ^ atomic_load_explicit(bar, memory_order_consume) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Fir_test.Env.empty_env) ;
      [%expect
        {|
        0
        0 & -209
        -15464318 ^ 1 - 1 & (0 | -18)
        268435456 ^ 268435456
        0 & (0 & 0) ^ -50348097 |}]

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type
        (fun e ->
          ( module Src.Expr.Int_values (struct
            let env = e
          end) ))
        Fir.Type.(int ())

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Int_values (struct
            let env = e
          end) ))
  end )

let%test_module "Int zeroes" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      print_sample
        ( module Src.Expr.Int_zeroes (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
          0
          0 & atomic_fetch_xor_explicit(&y, 0, memory_order_seq_cst)
          0 & atomic_fetch_add_explicit(&y, 99 & 0, memory_order_relaxed)
          4 & 0 |}]

    let test_fun (env : Fir.Env.t) :
        (module Q.Test.S with type t = Fir.Expression.t) =
      ( module Src.Expr.Int_zeroes (struct
        let env = env
      end) )

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type test_fun Fir.Type.(int ())

    let%test_unit "all expressions evaluate to 0" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Fir.Constant.as_int x with Ok 0 -> true | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Int_zeroes (struct
            let env = e
          end) ))
  end )

let%test_module "Atomic int nops" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      print_sample
        ( module struct
          module E = struct
            let env = env
          end

          module Af = Src.Expr.Atomic_fetch_int_nops (E) (E)

          (* TODO(@MattWindsor91): this is a hack. *)
          include Src.Expr.Int_values (E)

          let quickcheck_generator =
            Base_quickcheck.Generator.map ~f:Fir.Expression.atomic_fetch
              Af.quickcheck_generator

          let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
          atomic_fetch_and_explicit(bar, 95, memory_order_consume)
          atomic_fetch_add_explicit(bar, 53 -
                                    atomic_fetch_add_explicit(&y, 0,
                                                              memory_order_seq_cst),
                                    memory_order_relaxed)
          atomic_fetch_xor_explicit(&x, 0, memory_order_consume)
          atomic_fetch_or_explicit(&x, 27, memory_order_acquire)
          atomic_fetch_add_explicit(&y, 0, memory_order_acquire)
          atomic_fetch_and_explicit(&y, 53, memory_order_relaxed) |}]

    let test_fun (env : Fir.Env.t) :
        (module Q.Test.S with type t = Fir.Expression.t) =
      ( module Src.Expr.Int_zeroes (struct
        let env = env
      end) )

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type test_fun Fir.Type.(int ())

    (* TODO(@MattWindsor91): somehow check that these are indeed nops? *)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Int_zeroes (struct
            let env = e
          end) ))
  end )

let%test_module "Bool_values" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      print_sample
        ( module Src.Expr.Bool_values (struct
          let env = env
        end) )

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
      barbaz
      atomic_load_explicit(&z, memory_order_consume)
      atomic_load_explicit(&z, memory_order_seq_cst)
      foo > (*blep | 0 | 0)
      (atomic_load_explicit(bar, memory_order_relaxed) -
       atomic_load_explicit(bar, memory_order_relaxed) |
       (atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) | 0) & 0 |
       atomic_load_explicit(&x, memory_order_consume))
      == (0 & (27 - atomic_load_explicit(&x, memory_order_relaxed) | -1048576)) &&
      ((53 ^ atomic_load_explicit(&y, memory_order_acquire)) >= (0 & *blep | foo)
       || 0 != foo)
      *blep - *blep <= foo && *blep <= 53 -
      atomic_load_explicit(&y, memory_order_relaxed) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Fir_test.Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        atomic_load_explicit(foobaz, memory_order_consume)
        atomic_load_explicit(foobaz, memory_order_seq_cst)
        atomic_load_explicit(bar, memory_order_consume) !=
        (0 & atomic_load_explicit(bar, memory_order_acquire))
        atomic_load_explicit(bar, memory_order_consume) <
        atomic_load_explicit(bar, memory_order_acquire)
        ((atomic_load_explicit(bar, memory_order_consume) | 0 &
          (0 & atomic_fetch_sub_explicit(bar, 0, memory_order_relaxed)))
         & atomic_load_explicit(bar, memory_order_relaxed))
        < atomic_load_explicit(bar, memory_order_seq_cst)
        (atomic_fetch_sub_explicit(bar,
                                   atomic_load_explicit(bar, memory_order_relaxed) -
                                   atomic_load_explicit(bar, memory_order_relaxed),
                                   memory_order_acquire)
         ^ 0 & (95 - atomic_load_explicit(bar, memory_order_relaxed) | -1048576))
        < (95 & 0) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Fir_test.Env.empty_env) ;
      [%expect
        {|
        false
        (0 & 20) > (0 & 255)
        (134217728 ^ (0 ^ 0)) <= -31235266
        (true && false || (-15623063 & 0) > (-1 ^ -1)) && ((0 | 0) <= -1 || !true)
        !(0 != -23556581)
        !!((126030 | -2147483648 ^ -2147483648) < (-2147483648 ^ -2147483648)) |}]

    let test_fun (env : Fir.Env.t) =
      ( module Src.Expr.Bool_values (struct
        let env = env
      end) : Q.Test.S
        with type t = Fir.Expression.t )

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Fir.Type.(bool ())

    (* TODO(@MattWindsor91): we can't currently check that all expressions
       evaluate safely to Booleans, as the evaluator and known values tracker
       don't understand atomic loads. *)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Bool_values (struct
            let env = e
          end) ))
  end )

let%test_module "Bool falsehoods" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      let module B = Src.Expr.Bool_known (struct
        let env = env
      end) in
      print_sample (module B.Falsehoods)

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
          false
          false && atomic_load_explicit(&y, memory_order_seq_cst) <= foo
          true &&
          (!(barbaz >= true) && (0 & atomic_load_explicit(bar, memory_order_acquire))
           >=
           ((53 ^ atomic_load_explicit(&y, memory_order_acquire)) & (0 & *blep | foo)))
          (95 < atomic_load_explicit(bar, memory_order_seq_cst) || true > barbaz &&
           true)
          && (!atomic_load_explicit(&z, memory_order_seq_cst) || barbaz)
          !(true || barbaz) && (0 > 0 || 0 >= *blep)
          !(!(99 != *blep) || false) &&
          atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst) !=
          atomic_fetch_xor_explicit(&x, 126030 & 0, memory_order_acquire) |}]

    let test_fun (env : Fir.Env.t) :
        (module Q.Test.S with type t = Fir.Expression.t) =
      let module K = Src.Expr.Bool_known (struct
        let env = env
      end) in
      (module K.Falsehoods)

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Fir.Type.(bool ())

    let%test_unit "all expressions evaluate to 'false'" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Fir.Constant.as_bool x with Ok b -> not b | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Bool_values (struct
            let env = e
          end) ))
  end )

let%test_module "Bool tautologies" =
  ( module struct
    let print_sample (env : Fir.Env.t) =
      let module B = Src.Expr.Bool_known (struct
        let env = env
      end) in
      print_sample (module B.Tautologies)

    let%expect_test "sample" =
      print_sample (Lazy.force Fir_test.Env.test_env) ;
      [%expect
        {|
          true
          true || atomic_load_explicit(&y, memory_order_seq_cst) <= foo
          true ||
          (!(barbaz > true) || (0 & atomic_load_explicit(bar, memory_order_acquire)) >=
           ((53 ^ atomic_load_explicit(&y, memory_order_acquire)) & (0 & *blep | foo)))
          95 <= atomic_load_explicit(bar, memory_order_seq_cst) &&
          (true >= barbaz || true) ||
          (!atomic_load_explicit(&z, memory_order_seq_cst) || barbaz)
          !(false && barbaz) || (0 > 0 || 0 >= *blep)
          !(!(99 == *blep) && false) ||
          atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst) !=
          atomic_fetch_xor_explicit(&x, 126030 & 0, memory_order_acquire) |}]

    let test_fun (env : Fir.Env.t) :
        (module Q.Test.S with type t = Fir.Expression.t) =
      let module K = Src.Expr.Bool_known (struct
        let env = env
      end) in
      (module K.Tautologies)

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Fir.Type.(bool ())

    let%test_unit "all expressions evaluate to 'true'" =
      test_all_expressions_evaluate test_fun ~pred:(fun x ->
          match Fir.Constant.as_bool x with Ok b -> b | _ -> false)

    let%test_unit "all referenced variables in environment" =
      test_all_expressions_in_env (fun e ->
          ( module Src.Expr.Bool_values (struct
            let env = e
          end) ))
  end )
