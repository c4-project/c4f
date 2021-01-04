(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "bop_with_output" =
  ( module struct
    let print_sample ?(ops : Fir.Op.Binary.t list option)
        (out : Fir.Op_rule.Out.t) : unit =
      let x = Fir.Expression.of_variable_str_exn "x" in
      let y = Fir.Expression.of_variable_str_exn "y" in
      let gen =
        Option.value_exn
          (Src.Op.bop_with_output ?ops out)
          Src.Op.basic_lift_k
          (Two (x, y))
      in
      Act_utils.My_quickcheck.print_sample
        ~printer:(Fmt.pr "@[%a@]@." Act_litmus_c.Reify_expr.pp)
        ( module struct
          type t = Fir.Expression.t [@@deriving compare, sexp]

          let quickcheck_generator = gen

          let quickcheck_observer = Fir.Expression.quickcheck_observer

          let quickcheck_shrinker = Q.Shrinker.atomic
        end )

    let%expect_test "sample: any operator returning idempotence" =
      print_sample Idem ;
      [%expect
        {|
      0 + y
      x + 0
      y + 0
      y - 0
      -1 & y
      x & y
      0 | y
      x | 0
      y | 0
      0 ^ y
      y ^ 0
      x && y
      y && true
      y && x
      y || false |}]

    let%expect_test "sample: any operator returning zero" =
      print_sample Fir.Op_rule.Out.zero ;
      [%expect
        {|
      x - y
      y - x
      0 & x
      0 & y
      y & 0
      x ^ y
      y ^ x |}]

    let%expect_test "sample: any operator returning true" =
      print_sample (Const Fir.Constant.truth) ;
      [%expect
        {|
      x == y
      y == x
      y >= x
      x <= y
      y <= x
      true || x
      true || y
      y || true |}]

    let%expect_test "sample: any operator returning false" =
      print_sample (Const Fir.Constant.falsehood) ;
      [%expect
        {|
      x != y
      y != x
      y > x
      x < y
      y < x
      false && x
      false && y
      y && false |}]
  end )

let%test_module "operator rules" =
  ( module struct
    let test (type o) (module O : Fir.Op_types.S_binary with type t = o)
        (lift_o : O.t -> Fir.Op.Binary.t)
        (gen_k : Fir.Constant.t Base_quickcheck.Generator.t)
        ~(here : Lexing.position) : unit =
      Base_quickcheck.Test.run_exn
        ( module struct
          type t = O.t * Fir.Constant.t * Fir.Op_rule.t [@@deriving sexp]

          let quickcheck_generator : t Base_quickcheck.Generator.t =
            Base_quickcheck.Generator.(
              Let_syntax.(
                let%bind o = Base_quickcheck.Generator.of_list O.all in
                let%bind k = gen_k in
                let%map r = of_list (O.rules o) in
                (o, k, r)))

          let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
            [%quickcheck.shrinker:
              [%custom Base_quickcheck.Shrinker.atomic]
              * Fir.Constant.t
              * [%custom Base_quickcheck.Shrinker.atomic]]
        end )
        ~f:(fun (o, k, {Fir.Op_rule.in_; out_}) ->
          let kexp = Fir.Expression.constant k in
          let lexp, rexp =
            match in_ with
            | Const (Left, kl) ->
                (Fir.Expression.constant kl, kexp)
            | Const (Right, kr) ->
                (kexp, Fir.Expression.constant kr)
            | Refl ->
                (kexp, kexp)
          in
          let oconst = match out_ with Const ko -> ko | Idem -> k in
          let exp = Fir.Expression.bop (lift_o o) lexp rexp in
          [%test_eq: Fir.Constant.t Or_error.t] ~here:[[%here]; here]
            (Ok oconst)
            (Fir.Expression_eval.as_constant exp ~env:(Fir.Heap.empty ())))

    let%test_unit "arithmetic operators" =
      test
        (module Fir.Op.Binary.Arith)
        (fun x -> Fir.Op.Binary.Arith x)
        Fir.Constant.gen_int32 ~here:[%here]

    let%test_unit "bitwise operators" =
      test
        (module Fir.Op.Binary.Bitwise)
        (fun x -> Fir.Op.Binary.Bitwise x)
        Fir.Constant.gen_int32 ~here:[%here]

    let%test_unit "logical operators" =
      test
        (module Fir.Op.Binary.Logical)
        (fun x -> Fir.Op.Binary.Logical x)
        Fir.Constant.gen_bool ~here:[%here]

    let%test_unit "relational operators" =
      test
        (module Fir.Op.Binary.Rel)
        (fun x -> Fir.Op.Binary.Rel x)
        Fir.Constant.quickcheck_generator ~here:[%here]
  end )
