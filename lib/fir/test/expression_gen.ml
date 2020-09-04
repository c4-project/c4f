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
      atomic_load_explicit(bar, memory_order_consume)
      atomic_load_explicit(&x, memory_order_seq_cst)
      *blep ^ 99 |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0
        atomic_load_explicit(bar, memory_order_seq_cst)
        0 | (atomic_fetch_xor_explicit(bar, 0, memory_order_acq_rel) | 0 ^ 0) &
        ((6126 ^ 0) & atomic_load_explicit(bar, memory_order_relaxed))
        470264907 ^ 0 | -3937
        atomic_fetch_or_explicit(bar, 0, memory_order_consume) ^ 95 |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        -3937
        -1
        0
        3 ^ 3
        470264907 ^ 0 |}]

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
          atomic_load_explicit(bar, memory_order_consume) -
          atomic_load_explicit(bar, memory_order_consume)
          foo ^ 4 |}]

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
          atomic_fetch_and_explicit(bar, 95, memory_order_consume)
          atomic_fetch_add_explicit(bar, 53 & 0, memory_order_relaxed)
          atomic_fetch_or_explicit(&x, 27, memory_order_acquire)
          atomic_fetch_add_explicit(&y, 0, memory_order_acquire)
          atomic_fetch_and_explicit(&y, 53, memory_order_relaxed) |}]

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
      atomic_load_explicit(bar, memory_order_consume) ==
      atomic_load_explicit(&x, memory_order_seq_cst)
      (foo & 0) == 18140
      -190264 != foo
      !barbaz |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        (atomic_load_explicit(bar, memory_order_relaxed) ^
         atomic_load_explicit(bar, memory_order_relaxed))
        == atomic_fetch_xor_explicit(bar, 1129 & 0, memory_order_consume)
        atomic_fetch_sub_explicit(bar, 0, memory_order_seq_cst) != -5530953
        10703535 - 10703535 != 918
        (atomic_fetch_add_explicit(bar,
                                   atomic_load_explicit(bar, memory_order_seq_cst) -
                                   atomic_load_explicit(bar, memory_order_seq_cst),
                                   memory_order_seq_cst)
         - 95 & (95 ^ atomic_load_explicit(bar, memory_order_acquire)))
        == atomic_fetch_add_explicit(bar, 0 & -1, memory_order_relaxed) && true
        0 != -502 || 0 != atomic_fetch_or_explicit(bar, 0, memory_order_relaxed) ||
        (false || 6126 != 0) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env) ;
      [%expect
        {|
        14202 == ((1508833144 ^ 1508833144) & -2147483648 - -2147483648)
        (0 & 180118134) == (0 & 238948 & (0 ^ 411341))
        -190264 != -5530953
        (441 ^ 441) != 1129
        (-149248401 | -502) == (0 | 2147483647) && 1136 - 1136 == (-13666 & 0) |}]

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
          (barbaz || atomic_load_explicit(bar, memory_order_consume) != 0) && !barbaz
          && (0 != 0 && false && true) |}]

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
          !false && (!false || true)
          true || barbaz
          atomic_load_explicit(bar, memory_order_consume) -
          atomic_load_explicit(bar, memory_order_consume) !=
          atomic_load_explicit(&y, memory_order_relaxed) || (0 != 0 || barbaz) ||
          (true && true || !true) |}]

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
