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
      (-209 == atomic_load_explicit(&x, memory_order_consume))
      (89301152 == foo)
      (foo == atomic_load_explicit(&y, memory_order_relaxed))
      (atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(&y, memory_order_relaxed))
      (false && (true || atomic_load_explicit(&x, memory_order_seq_cst) == foo) &&
       barbaz || barbaz)
      (true && barbaz && true)
      (barbaz && barbaz || foo == atomic_load_explicit(&x, memory_order_seq_cst))
      (barbaz && foo == 1234853 && barbaz && true && (false || true) && false &&
       (((-860 == atomic_load_explicit(&y, memory_order_seq_cst)) && 19482 ==
         atomic_load_explicit(&x, memory_order_consume))
        || (*blep == *blep) || true)
       && false)
      ((barbaz || foo == 7998) && true)
      (false ||
       (true && (-5 == *blep) ||
        (((foo == *blep) && atomic_load_explicit(bar, memory_order_seq_cst) ==
          atomic_load_explicit(bar, memory_order_consume))
         && false && (1 == -6530420) || true && false)
        || barbaz && *blep == 409507680)
       || -50411168 == atomic_load_explicit(&y, memory_order_relaxed))
      (barbaz || barbaz) |}]

    let%expect_test "sample (environment has only atomic_int*)" =
      print_sample (Lazy.force Env.test_env_atomic_ptrs_only_mod) ;
      [%expect
        {|
      (false)
      (true)
      (-4 == -2147483648)
      (1 == 221)
      (1129 == atomic_load_explicit(bar, memory_order_seq_cst))
      (atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(bar, memory_order_consume))
      (true && (atomic_load_explicit(bar, memory_order_consume) == -112015996) &&
       22551631 == 33417)
      ((-607070849 == atomic_load_explicit(bar, memory_order_acquire)) &&
       atomic_load_explicit(bar, memory_order_seq_cst) == 0)
      (((((31453 == -18) || false) &&
         (57357 == atomic_load_explicit(bar, memory_order_relaxed)) &&
         atomic_load_explicit(bar, memory_order_seq_cst) == -860)
        && atomic_load_explicit(bar, memory_order_seq_cst) == 10703535)
       && true)
      (((((-1853 == -2147483648) ||
          ((atomic_load_explicit(bar, memory_order_seq_cst) == 437078) || true &&
           atomic_load_explicit(bar, memory_order_relaxed) ==
           atomic_load_explicit(bar, memory_order_consume))
          || (-2054658 == atomic_load_explicit(bar, memory_order_acquire)) &&
          (true &&
           ((atomic_load_explicit(bar, memory_order_consume) ==
             atomic_load_explicit(bar, memory_order_relaxed))
            || (148054 == atomic_load_explicit(bar, memory_order_seq_cst)) &&
            ((-21589512 == atomic_load_explicit(bar, memory_order_acquire)) ||
             false || false)
            && false && atomic_load_explicit(bar, memory_order_relaxed) ==
            atomic_load_explicit(bar, memory_order_acquire) && 4005 ==
            atomic_load_explicit(bar, memory_order_seq_cst))
           || false && true && (-22632 == 735443787) ||
           (atomic_load_explicit(bar, memory_order_seq_cst) ==
            atomic_load_explicit(bar, memory_order_acquire))
           &&
           (atomic_load_explicit(bar, memory_order_seq_cst) ==
            atomic_load_explicit(bar, memory_order_consume))
           || (-4 == atomic_load_explicit(bar, memory_order_consume)) &&
           ((-844745271 == 103) || 141 == -9) && false)
          || true)
         && (-1394 == atomic_load_explicit(bar, memory_order_relaxed)) &&
         (-5175 == -25) || 222 == atomic_load_explicit(bar, memory_order_relaxed))
        || -2147483648 == atomic_load_explicit(bar, memory_order_relaxed))
       && false || atomic_load_explicit(bar, memory_order_seq_cst) ==
       atomic_load_explicit(bar, memory_order_seq_cst))
      ((((atomic_load_explicit(bar, memory_order_acquire) == 918) || 89301152 ==
         atomic_load_explicit(bar, memory_order_seq_cst))
        || (-849891061 == atomic_load_explicit(bar, memory_order_relaxed)) ||
        atomic_load_explicit(bar, memory_order_relaxed) ==
        atomic_load_explicit(bar, memory_order_relaxed))
       && atomic_load_explicit(bar, memory_order_seq_cst) == -1315491)
      ((-38751544 == atomic_load_explicit(bar, memory_order_acquire)) ||
       (51 == -152110758) || false)
      ((false && atomic_load_explicit(bar, memory_order_relaxed) == -106378261) ||
       false && atomic_load_explicit(bar, memory_order_seq_cst) == 13)
      ((true && false ||
        (((151317 == atomic_load_explicit(bar, memory_order_consume)) &&
          ((atomic_load_explicit(bar, memory_order_seq_cst) ==
            atomic_load_explicit(bar, memory_order_consume))
           || false || false || -1 ==
           atomic_load_explicit(bar, memory_order_consume))
          || true && false)
         ||
         ((906 == atomic_load_explicit(bar, memory_order_seq_cst)) &&
          (true || 0 == 7633242 || false) && 140766 == -512041820)
         && false || 74 == atomic_load_explicit(bar, memory_order_seq_cst))
        || atomic_load_explicit(bar, memory_order_acquire) ==
        atomic_load_explicit(bar, memory_order_consume))
       || true || atomic_load_explicit(bar, memory_order_seq_cst) == -18166770)
      (((false &&
         ((-653346809 == 3618724) ||
          ((atomic_load_explicit(bar, memory_order_consume) == 11) || true) &&
          (((true &&
             ((((10 == atomic_load_explicit(bar, memory_order_seq_cst)) && 228880
                == -43)
               ||
               (((7301350 == 24) || false) &&
                (atomic_load_explicit(bar, memory_order_relaxed) == 129587862) &&
                false)
               || -1 == 3)
              && false || true && false)
             || (-1450 == -2147483648) && false &&
             (((209198417 == atomic_load_explicit(bar, memory_order_relaxed)) ||
               -368 == -325112898)
              && (2147483647 == 2147483647) || true)
             && (-2 == atomic_load_explicit(bar, memory_order_relaxed)) || false)
            ||
            (atomic_load_explicit(bar, memory_order_acquire) ==
             atomic_load_explicit(bar, memory_order_consume))
            || true && (-118017 == atomic_load_explicit(bar, memory_order_consume))
            || false)
           && (11094005 == atomic_load_explicit(bar, memory_order_consume)) &&
           false)
          || true &&
          ((true && true) ||
           (4919468 == atomic_load_explicit(bar, memory_order_seq_cst)) && 0 ==
           atomic_load_explicit(bar, memory_order_relaxed))
          || -15642 == atomic_load_explicit(bar, memory_order_seq_cst))
         && false)
        ||
        (true || true && (79870065 == 48611) ||
         ((19182 == -246629305) && false ||
          (-42 == atomic_load_explicit(bar, memory_order_consume)) || false ||
          ((true && (true || true) && true) || 10 == 931410594) &&
          (atomic_load_explicit(bar, memory_order_acquire) == -124515) ||
          atomic_load_explicit(bar, memory_order_consume) ==
          atomic_load_explicit(bar, memory_order_acquire))
         || (-536 == atomic_load_explicit(bar, memory_order_acquire)) || false)
        && (true || false && true) && 370936670 ==
        atomic_load_explicit(bar, memory_order_acquire))
       || true || true && 87005 == atomic_load_explicit(bar, memory_order_relaxed)) |}]

    let%expect_test "sample (environment is empty)" =
      print_sample (Lazy.force Env.empty_env_mod) ;
      [%expect
        {|
      (false)
      (true)
      (-849891061 == 1308564345)
      (-393099 == 59642983)
      (-1386 == 1066463)
      (false && ((false && false) || -50348097 == 10703535) || true)
      (true && (-186 == -22537) && true)
      ((-1394 == 6356718) &&
       ((2017411910 == 10) || ((4330 == 0) || 302886976 == -1) ||
        ((((((false &&
              ((-653346809 == 3618724) ||
               ((-23 == 5) || (-1461030106 == -2) && -996 == 16) &&
               ((2 == 14) || false) || (43718117 == 39984) || -2147483648 ==
               -3339703)
              && (-1 == 3) && false || true && false)
             || (-1450 == -2147483648) && false &&
             ((((-66190 == -64040) || true) || false) && ((792717104 == 2) || true)
              || -2 == 3127868)
             && (-22360 == -131107102) || (false || false) && (-165 == -35583) ||
             true)
            ||
            ((false || true && ((true && true) || false && false) || false) &&
             false)
            || 111008 == -15798577)
           && true || -89155514 == -306472)
          || true && (79870065 == 48611) ||
          ((19182 == -246629305) && false || (-42 == -949) || false) ||
          (true && ((-1 == -562451) || false) &&
           ((false || 235372 == -124515) && -5 == 11269657) && -635845724 ==
           191399)
          || 26195 == 5)
         && (-4 == -21) && (true && -2147483648 == 2) ||
         ((((-1744268 == -229322793) || false &&
            (((-1751473 == -2136) || false) || false) || -55399977 == 6839)
           && (false || true && true && false || false) && (-1 == 444) || 21823898
           == 993936)
          || true || true)
         && false)
        ||
        ((false && (-8 == 16) || ((-25993139 == 2502729) || true) ||
          (true || true || (2028265277 == 2971972) || 58067 == -1910 ||
           ((12 == -6675252) || -1 == -2147483648) && false && true ||
           (((657 == -1573123) && true) && true) && -2147483648 == 982)
          && ((289 == -318149970) && true && true) || -149490 == -15076697)
         || (114 == 529027985) && ((-12063219 == -92875855) && 0 == 186) ||
         (false || -480 == 3647 ||
          (((27 == 69346) || true) && ((1032398 == -749) || false) || 5156 == -29)
          && ((1859712709 == -1) && -2147483648 == 310601768) || true || false ||
          false || (-1596 == 14621) && false)
         && true)
        && 8563605 == 0)
       && ((-96005316 == 811) && 57164 == 801391) ||
       ((-19589 == -1438744) && false) && true)
      (((906 == -1) && -50411168 == 1414260) && true || 0 == 988777)
      ((true || 2147483647 == -18166770) && (-38751544 == 2375369) ||
       ((((true && ((false && false) || true) || (false && -2147483648 == 28) ||
           -144 == 143532)
          || false)
         ||
         (((((-1853 == -2147483648) || ((-149 == 3838553) || true) || 5 == 1569342)
            && true && true)
           ||
           (((((-1 == -6637079) && -2147483648 == -3705786) || (39028 == -21589512)
              || 565 == -766)
             || false)
            || (((-242387 == 1676) || -19170 == 2147483647) && false) && -22632 ==
            735443787)
           || (1 == -7896625) && true)
          && -2399 == 74672525)
         && true)
        && true)
       && (-844745271 == 103) || 141 == -9)
      ((true || (((-2147483648 == -235) || true) && false) || false) && false)
      (((true && -15464318 == -2147483648) || true && 18140 == -1) && true && 0 ==
       2134798 && true)
      ((((45766 == -1) && true) || 10 == -3164) && false ||
       (((151317 == -7613275) && -16098 == -1533) || false) || false ||
       (495683107 == -1377240818) && false)
      ((((48 == 126030) || false) || false) && false || true)
      (true || -825649965 == 139)
      (true || -1007 == -3 || -444183101 == 84) |}]

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
          (foo == 4)
          (*blep == 99)
          (atomic_load_explicit(&z, memory_order_seq_cst) == false)
          (true &&
           (((barbaz &&
              ((*blep == atomic_load_explicit(&y, memory_order_seq_cst)) || true) ||
              barbaz || false && 371254 ==
              atomic_load_explicit(bar, memory_order_relaxed) || (barbaz || barbaz) &&
              false)
             || atomic_load_explicit(&z, memory_order_seq_cst) == false)
            || barbaz)
           && true && barbaz || atomic_load_explicit(bar, memory_order_seq_cst) == 95
           || (atomic_load_explicit(bar, memory_order_relaxed) == foo) &&
           (-149 == atomic_load_explicit(&x, memory_order_relaxed)) || foo ==
           atomic_load_explicit(&x, memory_order_relaxed) || barbaz || false)
          ((atomic_load_explicit(&y, memory_order_seq_cst) == 53) &&
           atomic_load_explicit(&x, memory_order_seq_cst) == 27)
          (true && atomic_load_explicit(foobaz, memory_order_seq_cst) && true)
          ((barbaz || barbaz || true && true && true) &&
           atomic_load_explicit(&y, memory_order_seq_cst) == 53)
          ((barbaz || atomic_load_explicit(foobaz, memory_order_seq_cst) == true ||
            barbaz)
           && (atomic_load_explicit(&y, memory_order_seq_cst) == 53) &&
           atomic_load_explicit(foobaz, memory_order_seq_cst) == true)
          (false || (-9790791 == foo) || barbaz ||
           ((atomic_load_explicit(&y, memory_order_seq_cst) == 53) &&
            atomic_load_explicit(&x, memory_order_seq_cst) == 27)
           && true || true || atomic_load_explicit(&y, memory_order_acquire) ==
           atomic_load_explicit(&x, memory_order_relaxed))
          (true || barbaz)
          (barbaz || barbaz || (foo == *blep) || barbaz && 9 == foo)
          ((atomic_load_explicit(foobaz, memory_order_seq_cst) == true) || false)
          ((atomic_load_explicit(&x, memory_order_relaxed) == foo) ||
           ((atomic_load_explicit(bar, memory_order_acquire) == *blep) ||
            (false && barbaz) || true)
           || foo == atomic_load_explicit(bar, memory_order_consume))
          ((true && (-5 == *blep) ||
            (((foo == *blep) && atomic_load_explicit(bar, memory_order_seq_cst) ==
              atomic_load_explicit(bar, memory_order_consume))
             && false && (1 == -6530420) || true && false)
            || barbaz && *blep == 409507680)
           || atomic_load_explicit(&x, memory_order_seq_cst) == 27)
          (((barbaz || false) && true &&
            atomic_load_explicit(bar, memory_order_seq_cst) ==
            atomic_load_explicit(&x, memory_order_consume))
           || ((atomic_load_explicit(bar, memory_order_seq_cst) == 95) && true) || foo
           == atomic_load_explicit(&x, memory_order_seq_cst))
          (atomic_load_explicit(foobaz, memory_order_seq_cst) || barbaz ||
           (atomic_load_explicit(&x, memory_order_consume) ==
            atomic_load_explicit(&x, memory_order_relaxed))
           && barbaz || false || true || false && (barbaz || barbaz) &&
           (-50348097 == 10703535) && true)
          ((((((true && ((foo == foo) || true) || (148054 == foo) && (-954029 == -124)
                && foo == -2 &&
                ((foo == atomic_load_explicit(bar, memory_order_relaxed)) && barbaz) &&
                false)
               || barbaz)
              || barbaz)
             || barbaz)
            ||
            (((true || true && barbaz ||
               (((((atomic_load_explicit(bar, memory_order_relaxed) == -1) || -354132
                   == atomic_load_explicit(&y, memory_order_seq_cst))
                  && true)
                 || true)
                && barbaz && barbaz && true)
               || *blep == 11343 || false)
              && atomic_load_explicit(foobaz, memory_order_seq_cst))
             && false || barbaz || atomic_load_explicit(bar, memory_order_seq_cst) ==
             95)
            && atomic_load_explicit(&x, memory_order_seq_cst) == 27)
           || false || true || (atomic_load_explicit(bar, memory_order_consume) == foo)
           && (-3531150 == -64) && barbaz) |}]

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
