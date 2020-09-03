(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fir
  module Q = Base_quickcheck
  module Qx = Act_utils.My_quickcheck
end

let printer : Src.Expression.t -> unit =
  Fmt.(pr "@[%a@]@." Act_litmus_c.Reify_expr.pp)

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

module Exp_lvalues =
  Travesty.Traversable.Chain0
    (Act_fir.Expression_traverse.On_addresses)
    (Act_fir.Address.On_lvalues)
module Exp_idents =
  Travesty.Traversable.Chain0 (Exp_lvalues) (Act_fir.Lvalue.On_identifiers)

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
      0
      atomic_load_explicit(&x, memory_order_seq_cst)
      foo - foo
      53 ^ atomic_load_explicit(&y, memory_order_consume)
      atomic_load_explicit(&x, memory_order_acquire) ^ 1129 |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0
        (-860 ^ atomic_load_explicit(bar, memory_order_relaxed) ^ 19482) &
        (atomic_fetch_sub_explicit(bar, 2147483647 - 2147483647,
                                   memory_order_acquire)
         ^ 0 & atomic_load_explicit(bar, memory_order_seq_cst) |
         atomic_load_explicit(bar, memory_order_seq_cst) ^
         atomic_load_explicit(bar, memory_order_acquire))
        95 ^ atomic_load_explicit(bar, memory_order_relaxed)
        95 ^ atomic_load_explicit(bar, memory_order_consume)
        atomic_fetch_xor_explicit(bar, -879720314 - -879720314, memory_order_consume)
        ^ (7471 | atomic_fetch_xor_explicit(bar, 0, memory_order_consume)) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        -3937
        0
        0 | (-859284515 | 7627 ^ 0) & ((124166343 | 0) ^ 10703535)
        0 ^ 0 | -2147483648
        22551631 ^ 22551631 |}]

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
          foo - foo
          53 ^ atomic_load_explicit(&y, memory_order_consume)
          57357 ^ 57357
          atomic_load_explicit(&y, memory_order_relaxed) ^ 53 |}]

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

let%test_module "Atomic int nops" =
  ( module struct
    let print_sample (env : Src.Env.t) =
      print_sample
        ( module struct
          module E = struct
            let env = env
          end

          module Af = Src.Expression_gen.Atomic_fetch_int_nops (E) (E)

          (* TODO(@MattWindsor91): this is a hack. *)
          include Src.Expression_gen.Int_values (E)

          let quickcheck_generator =
            Base_quickcheck.Generator.map ~f:Src.Expression.atomic_fetch
              Af.quickcheck_generator

          let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
        end )

    let%expect_test "sample" =
      print_sample (Lazy.force Env.test_env) ;
      [%expect
        {|
          atomic_fetch_sub_explicit(bar,
                                    atomic_fetch_or_explicit(bar, 53 ^
                                                             atomic_fetch_add_explicit
                                                             (&y, 0,
                                                              memory_order_acq_rel),
                                                             memory_order_relaxed)
                                    - 95, memory_order_seq_cst)
          atomic_fetch_sub_explicit(&x, 99 - *blep, memory_order_consume)
          atomic_fetch_add_explicit(&x, 53 ^
                                    atomic_load_explicit(&y, memory_order_consume),
                                    memory_order_relaxed)
          atomic_fetch_xor_explicit(&x, 57357 ^ 57357, memory_order_consume)
          atomic_fetch_add_explicit(&y, 0, memory_order_acquire) |}]

    let test_fun (env : Src.Env.t) :
        (module Q.Test.S with type t = Src.Expression.t) =
      ( module Src.Expression_gen.Int_zeroes (struct
        let env = env
      end) )

    let%test_unit "all expressions have 'int' type" =
      test_all_expressions_have_type test_fun Src.Type.(int ())

    (* TODO(@MattWindsor91): somehow check that these are indeed nops? *)

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
      barbaz
      -190264 == foo
      (22551631 ^ 22551631) == (-153 | 0)
      !((barbaz && 0 == 0 || 0 == 0 && true) &&
        (!false || atomic_load_explicit(bar, memory_order_relaxed) - 95 == *blep)) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        atomic_fetch_sub_explicit(bar, 0, memory_order_seq_cst) == -5530953
        atomic_load_explicit(bar, memory_order_consume) ==
        (atomic_load_explicit(bar, memory_order_acquire) - 95 & -245825 & 590 - 590 ^
         (0 ^ 95 - atomic_load_explicit(bar, memory_order_consume) &
          atomic_fetch_or_explicit(bar, 95 ^
                                   atomic_fetch_or_explicit(bar, 0,
                                                            memory_order_acquire),
                                   memory_order_relaxed)))
        (95 ^ atomic_load_explicit(bar, memory_order_acquire)) ==
        atomic_load_explicit(bar, memory_order_relaxed) - 95 &&
        (atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) -
         atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) == 0 && 2147483647
         == atomic_load_explicit(bar, memory_order_relaxed) - 95)
        (0 == 0 || atomic_load_explicit(bar, memory_order_consume) == 0) && !true
        0 == 22551631 || 0 == atomic_load_explicit(bar, memory_order_relaxed) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        -190264 == -5530953
        7998 == 0
        0 == 22551631 || 0 == -15464318
        -502 - -502 == (0 | 2147483647) || true && 0 == 57529197
        0 == (-4713 ^ 0) && (0 == 14202 && 0 == 1508833144) ||
        (0 ^ -2147483648 | -127350 ^ 0) == ((1330530 | 0) & (0 & -245825)) |}]

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
          (barbaz || atomic_load_explicit(bar, memory_order_consume) == 0) && barbaz &&
          ((false || false) && barbaz)
          !(atomic_load_explicit(&x, memory_order_seq_cst) == 27) |}]

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
          atomic_load_explicit(bar, memory_order_seq_cst) == 95
          atomic_load_explicit(&z, memory_order_seq_cst) == false
          true || barbaz
          (22551631 ^ 22551631) == (-153 | 0) || barbaz || true &&
          (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) == 27) |}]

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
