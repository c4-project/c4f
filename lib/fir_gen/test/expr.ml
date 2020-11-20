(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

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

let test_has_type (type_of : Fir.Expression.t -> Fir.Type.t Or_error.t)
    (e : Fir.Expression.t) (ty : Fir.Type.t) ~(here : Lexing.position) : unit
    =
  [%test_result: Fir.Type.t Or_error.t] (type_of e) ~here:[here; [%here]]
    ~equal:[%compare.equal: Fir.Type.t Or_error.t] ~expect:(Ok ty)

let test_in_env (env : Fir.Env.t) (e : Fir.Expression.t)
    ~(here : Lexing.position) : unit =
  [%test_pred: Fir.Expression.t]
    (Fir.Expression_traverse.On_addresses.for_all
       ~f:(Accessor.for_all Fir.Address.variable_of ~f:(Map.mem env)))
    ~here:[here; [%here]] e

let test_evaluates_to (heap : Fir.Heap.t) (e : Fir.Expression.t)
    (k : Fir.Constant.t) ~(here : Lexing.position) : unit =
  [%test_result: Fir.Constant.t Or_error.t]
    (Fir.Expression_eval.as_constant ~env:heap e)
    ~expect:(Ok k) ~here:[here; [%here]]

(** [test_all_expressions ?evaluate_to f ~have_type ~here] does a salvo of
    unit tests on a random expression generator; we do these all together
    because expression generators tend to be quite slow. *)
let test_all_expressions ?(evaluate_to : Fir.Constant.t option)
    (f : Fir.Env.t -> (module Q.Test.S with type t = Fir.Expression.t))
    ~(have_type : Fir.Type.t) ~(here : Lexing.position) =
  let env = Lazy.force Fir_test.Env.test_env in
  let heap = Fir.Heap.make (Fir.Address.eval_on_env ~env) in
  let (module Qc) = f env in
  let module Ty = Fir.Expression.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun e ->
      test_has_type ~here Ty.type_of e have_type ;
      test_in_env ~here env e ;
      Option.iter evaluate_to ~f:(test_evaluates_to ~here heap e))

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
      foo - 4
      atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) - 53 - (foo - 4)
      2147483647 |
      atomic_fetch_xor_explicit(bar, true ? foo - 4 :
                                atomic_fetch_and_explicit(bar,
                                                          atomic_fetch_add_explicit
                                                          (bar,
                                                           atomic_load_explicit
                                                           (bar,
                                                            memory_order_acquire)
                                                           - 95,
                                                           memory_order_release)
                                                          - 96,
                                                          memory_order_relaxed)
                                - 95, memory_order_consume)
      atomic_load_explicit(&y, memory_order_acquire) ^ foo | 0 & -1
      barbaz ? atomic_load_explicit(foobaz, memory_order_acquire) ?
      atomic_load_explicit(bar, memory_order_relaxed) - 95 :
      atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst) : true ?
      atomic_load_explicit(&x, memory_order_relaxed) - 27 : *blep - 99
      foo - 4 < 0 || barbaz ?
      atomic_fetch_xor_explicit(&y, false ?
                                atomic_fetch_or_explicit(&y, foo - 4,
                                                         memory_order_release)
                                : 0, memory_order_consume)
      : *blep | 0 |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Fir_test.Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        0
        atomic_load_explicit(bar, memory_order_seq_cst) - 95
        (atomic_load_explicit(bar, memory_order_acquire) - 95 ^
         atomic_load_explicit(bar, memory_order_acquire) - 95)
        &
        (atomic_load_explicit(bar, memory_order_seq_cst) &
         (atomic_load_explicit(bar, memory_order_consume) ^
          atomic_fetch_and_explicit(bar,
                                    atomic_load_explicit(bar, memory_order_relaxed) -
                                    96, memory_order_acq_rel)
          - 95))
        (atomic_fetch_sub_explicit(bar,
                                   atomic_load_explicit(bar, memory_order_consume) -
                                   95, memory_order_release)
         < atomic_load_explicit(bar, memory_order_relaxed) ?
         atomic_load_explicit(bar, memory_order_relaxed) - 95 :
         atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed))
        & atomic_load_explicit(bar, memory_order_relaxed) - 95
        atomic_fetch_and_explicit(bar, -1, memory_order_seq_cst) >
        atomic_load_explicit(bar, memory_order_consume) ?
        atomic_load_explicit(bar, memory_order_acquire) :
        atomic_fetch_or_explicit(bar, atomic_load_explicit(bar, memory_order_consume)
                                 - 95, memory_order_acquire)
        atomic_load_explicit(bar, memory_order_seq_cst) >
        atomic_load_explicit(bar, memory_order_seq_cst) ? 0 :
        atomic_load_explicit(bar, memory_order_relaxed) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Fir_test.Env.empty_env) ;
      [%expect
        {|
        0
        (false ? 0 : 0) & (134217728 | -15464318)
        (true ? 0 : -1) & (-502 & (0 | 1073741824))
        (true ? 0 : 13418) & (false ? 19482 : 0)
        0 ^ 0
        (18659626 >= 0 ? 0 ^ 0 : 0 + 0) ^ (0 == 0 ? -1 < 0 ? 0 : -2047 : -444183101) |}]

    let%test_unit "all expressions have 'int' type and vars are in \
                   environment" =
      test_all_expressions
        (fun e ->
          ( module Src.Expr.Int_values (struct
            let env = e
          end) ))
        ~have_type:Fir.Type.(int ())
        ~here:[%here]
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
          27 -
          atomic_fetch_sub_explicit(&x, foo - 4 -
                                    (atomic_load_explicit(bar, memory_order_consume) -
                                     95),
                                    memory_order_seq_cst)
          95 - atomic_load_explicit(bar, memory_order_consume)
          barbaz ? foo - 4 : 0
          barbaz ? atomic_load_explicit(&y, memory_order_consume) - 53 :
          atomic_fetch_sub_explicit(&y,
                                    atomic_fetch_xor_explicit(bar, *blep - 99,
                                                              memory_order_acquire)
                                    - 95 ^
                                    atomic_fetch_sub_explicit(&x, foo - 4,
                                                              memory_order_release)
                                    - 27, memory_order_acq_rel) |}]

    let%test_unit "all expressions are 'int' 0 and vars are in environment" =
      test_all_expressions
        (fun e ->
          ( module Src.Expr.Int_zeroes (struct
            let env = e
          end) ))
        ~have_type:Fir.Type.(int ())
        ~evaluate_to:(Int 0) ~here:[%here]
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
          atomic_fetch_add_explicit(bar,
                                    atomic_load_explicit(bar, memory_order_consume) -
                                    95, memory_order_acquire)
          atomic_fetch_or_explicit(&x, 27, memory_order_relaxed)
          atomic_fetch_or_explicit(&x, 27, memory_order_consume)
          atomic_fetch_or_explicit(&x, 27, memory_order_release)
          atomic_fetch_sub_explicit(&x, barbaz ?
                                    atomic_load_explicit(&y, memory_order_seq_cst) - 53
                                    : atomic_load_explicit(&y, memory_order_acquire),
                                    memory_order_release)
          atomic_fetch_and_explicit(&y, 53, memory_order_seq_cst) |}]

    let%test_unit "all expressions have 'int' type and vars are in \
                   environment" =
      test_all_expressions
        (fun env ->
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
          end ))
        ~have_type:Fir.Type.(int ())
        ~here:[%here]

    (* TODO(@MattWindsor91): somehow check that these are indeed nops? *)
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
      atomic_load_explicit(&z, memory_order_consume)
      atomic_load_explicit(&z, memory_order_seq_cst)
      (atomic_fetch_and_explicit(&y, *blep - 100, memory_order_relaxed) - 53 & foo)
      != foo
      *blep >
      (true ? atomic_load_explicit(bar, memory_order_acquire) :
       atomic_fetch_add_explicit(&x, atomic_load_explicit(&y, memory_order_acquire)
                                 - 53, memory_order_consume))
      || !(*blep == 2147483647)
      (true && atomic_load_explicit(&z, memory_order_consume) ||
       (false || atomic_load_explicit(bar, memory_order_acquire) <=
        atomic_load_explicit(bar, memory_order_consume) - 95))
      &&
      (atomic_fetch_or_explicit(bar, 0, memory_order_consume) |
       atomic_load_explicit(bar, memory_order_relaxed))
      <
      (barbaz ?
       atomic_fetch_xor_explicit(&x, atomic_load_explicit(&y, memory_order_consume)
                                 - 53, memory_order_acquire)
       : atomic_load_explicit(&x, memory_order_consume) - 27)
      || true |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Fir_test.Env.test_env_atomic_ptrs_only) ;
      [%expect
        {|
        atomic_load_explicit(foobaz, memory_order_consume)
        atomic_load_explicit(foobaz, memory_order_seq_cst)
        (atomic_load_explicit(foobaz, memory_order_relaxed) ? 0 :
         atomic_load_explicit(bar, memory_order_consume) - 95)
        ==
        (atomic_load_explicit(bar, memory_order_seq_cst) &
         (atomic_load_explicit(bar, memory_order_consume) ^
          atomic_fetch_and_explicit(bar,
                                    atomic_load_explicit(bar, memory_order_relaxed) -
                                    96, memory_order_acq_rel)
          - 95))
        (atomic_fetch_and_explicit(bar,
                                   atomic_load_explicit(bar, memory_order_acquire) -
                                   96, memory_order_relaxed)
         - 95 & atomic_load_explicit(bar, memory_order_acquire))
        <=
        (atomic_load_explicit(bar, memory_order_consume) - 95 &
         atomic_load_explicit(bar, memory_order_seq_cst))
        2147483647 <
        atomic_fetch_xor_explicit(bar, true ?
                                  atomic_load_explicit(bar, memory_order_consume) -
                                  95 :
                                  atomic_fetch_sub_explicit(bar,
                                                            atomic_fetch_add_explicit
                                                            (bar,
                                                             atomic_load_explicit
                                                             (bar,
                                                              memory_order_acquire)
                                                             - 95,
                                                             memory_order_release)
                                                            - 95,
                                                            memory_order_relaxed),
                                  memory_order_consume) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Fir_test.Env.empty_env) ;
      [%expect
        {|
        false
        true
        ((true ? -780780327 : 0) ^ 0 & 0) <= (0 & -9790791)
        (-378 | 0) >= -879720314 || (-1 == -2 && 7627 <= 0 || (0 > 0 || true)) ||
        false
        !(0 != 2147483647) |}]

    let%test_unit "all expressions have 'bool' type and vars are in \
                   environment" =
      test_all_expressions
        (fun e ->
          ( module Src.Expr.Bool_values (struct
            let env = e
          end) ))
        ~have_type:Fir.Type.(bool ())
        ~here:[%here]
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
          4 != foo
          false && *blep >= atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst)
          atomic_load_explicit(&z, memory_order_seq_cst) && false &&
          !(*blep == foo || atomic_load_explicit(&y, memory_order_relaxed) > *blep - 99
            || !atomic_load_explicit(&z, memory_order_relaxed))
          && true
          !(true || false || atomic_load_explicit(&y, memory_order_acquire) == foo) &&
          !(*blep > *blep - 99 && barbaz) |}]

    let%test_unit "all expressions evaluate to false and vars are in \
                   environment" =
      test_all_expressions
        (fun e ->
          let module B = Src.Expr.Bool_known (struct
            let env = e
          end) in
          (module B.Falsehoods))
        ~have_type:Fir.Type.(bool ())
        ~evaluate_to:(Bool false) ~here:[%here]
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
          4 == foo
          true || *blep >= atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst)
          atomic_load_explicit(&z, memory_order_seq_cst) || true ||
          !(*blep == foo || atomic_load_explicit(&y, memory_order_relaxed) > *blep - 99
            || !atomic_load_explicit(&z, memory_order_relaxed))
          || true
          !(false && false && atomic_load_explicit(&y, memory_order_acquire) == foo) ||
          !(*blep > *blep - 99 && barbaz) |}]

    let%test_unit "all expressions evaluate to true and vars are in \
                   environment" =
      test_all_expressions
        (fun e ->
          let module B = Src.Expr.Bool_known (struct
            let env = e
          end) in
          (module B.Tautologies))
        ~have_type:Fir.Type.(bool ())
        ~evaluate_to:(Bool true) ~here:[%here]
  end )
