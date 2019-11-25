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
  e |> Src.Reify_expr.reify
  |> Fmt.(pr "@[%a@]@." (parens Act_c_lang.Ast.Expr.pp))

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
      (barbaz)
      (18 == atomic_load_explicit(bar, memory_order_seq_cst))
      (*blep == 19482)
      (atomic_load_explicit(&y, memory_order_acquire) == -1)
      (true && atomic_load_explicit(bar, memory_order_acquire) == 906)
      (barbaz && (foo == -452191315 || foo == 7998))
      (barbaz && !(470264907 == -879720314))
      (atomic_load_explicit(bar, memory_order_seq_cst) == -1736309620 && foo ==
       atomic_load_explicit(&y, memory_order_relaxed))
      (((barbaz || barbaz && foo ==
         atomic_load_explicit(bar, memory_order_relaxed))
        && atomic_load_explicit(&x, memory_order_seq_cst) == foo)
       && !barbaz)
      ((barbaz && atomic_load_explicit(bar, memory_order_consume) == 1234853 ||
        barbaz)
       && true)
      (85943 == atomic_load_explicit(&x, memory_order_relaxed) || !false)
      (*blep == *blep || -38250 == foo)
      (atomic_load_explicit(bar, memory_order_acquire) == *blep || 454 == foo &&
       (12922 == *blep || barbaz || barbaz || foo == foo) || !(foo == *blep))
      (2869 == -3 && !((foo == *blep || barbaz) || -1344830 == 0 || false) || 1 ==
       -6530420)
      ((barbaz && barbaz || barbaz) || barbaz)
      (!(*blep == foo) || false)
      (!(!!barbaz && barbaz)) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (false)
      (true)
      (-34 == atomic_load_explicit(bar, memory_order_seq_cst))
      (-7 == 0)
      (69640 == atomic_load_explicit(bar, memory_order_seq_cst))
      (85943 == -43945)
      (atomic_load_explicit(bar, memory_order_acquire) ==
       atomic_load_explicit(bar, memory_order_acquire))
      (atomic_load_explicit(bar, memory_order_consume) == 3)
      (atomic_load_explicit(bar, memory_order_consume) == 488)
      (-15623063 == 2147483647 && (false || 6126 == -360539 || 10703535 == -4713))
      (-326 == atomic_load_explicit(bar, memory_order_seq_cst) &&
       (-149248401 == atomic_load_explicit(bar, memory_order_acquire) || true))
      (atomic_load_explicit(bar, memory_order_seq_cst) == -2147483648 &&
       atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(bar, memory_order_consume))
      (!((-2147483648 == -52859389 && false) &&
         !(-9790791 == atomic_load_explicit(bar, memory_order_consume)))
       &&
       !(atomic_load_explicit(bar, memory_order_relaxed) ==
         atomic_load_explicit(bar, memory_order_relaxed) &&
         atomic_load_explicit(bar, memory_order_seq_cst) == -1315491))
      (false || !(1 == -6530420))
      (true || -19 == 745)
      (atomic_load_explicit(bar, memory_order_acquire) == 57685 ||
       !((-1 == -1 &&
          !((!(!(atomic_load_explicit(bar, memory_order_acquire) ==
                 atomic_load_explicit(bar, memory_order_consume) ||
                 !(atomic_load_explicit(bar, memory_order_consume) == 9))
               &&
               !(true &&
                 (atomic_load_explicit(bar, memory_order_acquire) == -15863051 ||
                  atomic_load_explicit(bar, memory_order_relaxed) == 113305704))
               && 576814 == atomic_load_explicit(bar, memory_order_acquire))
             &&
             !(28 == atomic_load_explicit(bar, memory_order_relaxed) || -220 == 13)
             ||
             !((-1853 == -2147483648 ||
                !(atomic_load_explicit(bar, memory_order_seq_cst) == 437078 ||
                  atomic_load_explicit(bar, memory_order_relaxed) ==
                  atomic_load_explicit(bar, memory_order_consume) ||
                  atomic_load_explicit(bar, memory_order_relaxed) == -108572))
               &&
               !!!(-1 == atomic_load_explicit(bar, memory_order_consume) || -109 ==
                   -3705786))
             ||
             !(-954029 == atomic_load_explicit(bar, memory_order_consume) ||
               -1532663 == atomic_load_explicit(bar, memory_order_seq_cst)))
            &&
            (-29835015 == -242387 || -28 ==
             atomic_load_explicit(bar, memory_order_seq_cst) &&
             (-2556900 == -22632 ||
              !(atomic_load_explicit(bar, memory_order_relaxed) ==
                atomic_load_explicit(bar, memory_order_consume))))))
         && atomic_load_explicit(bar, memory_order_acquire) ==
         atomic_load_explicit(bar, memory_order_seq_cst)))
      (atomic_load_explicit(bar, memory_order_relaxed) == -5 || 1861 ==
       atomic_load_explicit(bar, memory_order_consume) ||
       atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(bar, memory_order_consume))
      (atomic_load_explicit(bar, memory_order_consume) == -112015996 || 22551631 ==
       33417)
      (!(atomic_load_explicit(bar, memory_order_consume) ==
         atomic_load_explicit(bar, memory_order_acquire)))
      (!!!false) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (true)
      (-112015996 == 441)
      (-14 == 3)
      (18 == 320421)
      (28 == 913948776)
      (4592 == 2017411910)
      (47969 == -29735)
      (143532 == 30638885)
      (576814 == 465)
      (-859284515 == 2134798 && -1 == -61683)
      (-970 == 1136190 &&
       (-7118 == -6964 || true &&
        (false && -15076697 == -9533616 &&
         !((-31 == 14200 || -92875855 == 1509925) || 3518 == -253734 &&
           ((((true && (65570610 == -38 || false)) && -2 == -10091) &&
             (62815 == 5156 || (6860 == 1859712709 || 602985 == -2147483648) || -9
              == 667549020 && -2013373 == -1)
             ||
             (1219 == 1662229 || (5 == 7 || 1496 == 3973) && -113196593 == -19589
              && 5606361 == -110390)
             && 1292381 == -2147483648)
            && true || 2147483647 == -1381651)
           && -2092912 == -825649965))
        &&
        !(((true && !(!(195063585 == -2) && -212770 == -22180214)) && 9799 == 0 &&
           1414 == 399948372)
          && -654309041 == 253879135)
        && true))
      ((-1853 == -2147483648 || !(-149 == 3838553 || -3207 == -2147483648)) &&
       (-2054658 == -17 &&
        ((true || !(-6637079 == -3005) ||
          (((80 == 306873 || -2027 == 0) &&
            (-29835015 == -242387 || 4005 == -19170))
           && -2556900 == -22632)
          && !(6507 == 1))
         || !(2147483647 == -1 || 488 == -1960)))
       && 11343 == 2052)
      (2 == -11918 || 0 == 104965949)
      (202286 == 140766 || -1 == -1 &&
       !((273 == -2009605104 && -2 == 9 || 48510554 == -3 && 51 == -152110758) &&
         false))
      ((-1 == 19482 && !(1 == 18659626) || 261759771 == -39848) &&
       (-674356 == -25128703 && !(94641 == 13585) ||
        !(1330530 == 180118134 && false))
       || !((-34 == 32212 && -1 == 61684972) && 1917247 == 0) &&
       ((-33683577 == -16098 || true && -1 == -1) || (7 == -2147483648 && true) &&
        -206449 == 409507680 && -50411168 == 1414260))
      (!((-2038 == 31 && true) && 359247 == 184) && 115269 == -87439 ||
       (!!(-46 == 38057) && true && -746 == -13937556 ||
        !(1413267 == -1 || 0 == 370582) || 1425738 == 30159)
       && 155 == 9542 || 541815 == -1)
      (!(69640 == 516) || !false)
      (!(85086 == 28933))
      (!((false ||
          !((!(-120495 == -12400315 ||
               ((6191823 == -2147483648 || false) ||
                !(true || !(-4884 == -964695275 && 26 == -199706)))
               && !(-43 == -41))
             ||
             ((10495822 == 1 && 129587862 == 49) && false ||
              (0 == -2147483648 || 334 == 374) && false && 15 == 209198417)
             ||
             !(((-2093708404 == -2495 && true) && 2147483647 == -4 ||
                !!(209957105 == -4 || -131107102 == 49))
               && 1826751 == 1772)
             &&
             (-270 == 468 &&
              ((16 == 4919468 || false) || 0 == 2147483647 && -15642 == 25846698))
             &&
             ((2147483647 == 2736 || -8917222 == -1556113709) || -16 == 38197 ||
              (true && -344578 == 9 || true || 1232905 == -562451) && 3583 == 2539))
            || !(-124515 == -214980))
          &&
          ((((302430 == 242 && -151920 == 186796963) &&
             !(-127695368 == -645529 && -1114576 == 2 &&
               !(16427893 == -3 && 325 == 273) && true && true || -585 == 538)
             || 6839 == -825974609)
            || true ||
            (64502772 == -686684577 &&
             !((-2147483648 == -50788434 || -82561647 == 7630213 && 253918237 == -1
                || -51580 == -313 && false)
               && -497 == -442)
             && !(!true || !((-159 == 2 || true) || -1910 == -22147 && false)))
            && 377824 == -1 && true)
           || -21669488 == -1305))
         && false))
      (!(30682120 == -326 || -3937 == -52910)) |}]

    let test_fun (module E : Src.Env_types.S_with_known_values) =
      (module Src.Expression_gen.Bool_values (E) : Q.Test.S
        with type t = Src.Expression.t )

    let%test_unit "all expressions have 'bool' type" =
      test_all_expressions_have_type test_fun Src.Type.(bool ())

    (* TODO(@MattWindsor91): we can't currently check that all expressions
       evaluate safely to Booleans, as the evaluator and known values tracker
       don't understand atomic loads. *)

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
          (barbaz)
          (*blep == 99)
          (atomic_load_explicit(foobaz, memory_order_seq_cst) == true)
          (atomic_load_explicit(&y, memory_order_seq_cst) == 53)
          (true && true)
          (barbaz == true && true)
          (foo == 4 && true)
          ((true && atomic_load_explicit(foobaz, memory_order_seq_cst)) && true)
          (false || -9790791 == foo ||
           (barbaz || ((true || !barbaz) && true) && (barbaz == true || barbaz)) ||
           !(atomic_load_explicit(&x, memory_order_relaxed) == foo))
          (true || true)
          (true || barbaz)
          (foo == -452191315 || atomic_load_explicit(foobaz, memory_order_seq_cst) ==
           true)
          (2869 == -3 && !((foo == *blep || barbaz) || -1344830 == 0 || false) ||
           atomic_load_explicit(bar, memory_order_seq_cst) == 95)
          ((barbaz || foo == atomic_load_explicit(bar, memory_order_seq_cst)) &&
           atomic_load_explicit(&y, memory_order_relaxed) == *blep && 470264907 ==
           -879720314 ||
           (atomic_load_explicit(&x, memory_order_seq_cst) == 27 ||
            atomic_load_explicit(bar, memory_order_consume) == 1234853)
           || barbaz)
          (((*blep == foo || true) ||
            !((barbaz || barbaz) || -50348097 == 10703535 ||
              atomic_load_explicit(&x, memory_order_consume) == 12062))
           || barbaz) |}]

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
