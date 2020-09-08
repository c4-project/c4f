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
end

let%test_module "bop" =
  ( module struct
    let print_sample (type t)
        (module M : Src.Op_types.S_binary with type t = t)
        (promote : t -> Src.Op.Binary.t) (out : Src.Op_rule.Out.t) : unit =
      let x = Src.Expression.of_variable_str_exn "x" in
      let y = Src.Expression.of_variable_str_exn "y" in
      let gen = Src.Op_gen.bop (module M) (Two (x, y)) ~promote ~out in
      Act_utils.My_quickcheck.print_sample
        ~printer:(Fmt.pr "@[%a@]@." Act_litmus_c.Reify_expr.pp)
        ( module struct
          type t = Src.Expression.t [@@deriving compare, sexp]

          let quickcheck_generator = gen

          let quickcheck_observer = Src.Expression.quickcheck_observer

          let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
        end )

    let%expect_test "sample: any operator returning idempotence" =
      print_sample (module Src.Op.Binary) Fn.id Idem ;
      [%expect
        {|
      0 + x
      y + 0
      y - 0
      0 | y
      x | 0
      y | 0
      y | x
      x && true
      y && true
      y && x
      false || x
      false || y
      x || false
      y || false
      y || x |}]

    let%expect_test "sample: any operator returning zero" =
      print_sample (module Src.Op.Binary) Fn.id Src.Op_rule.Out.zero ;
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
      print_sample (module Src.Op.Binary) Fn.id (Const Src.Constant.truth) ;
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
      print_sample
        (module Src.Op.Binary)
        Fn.id (Const Src.Constant.falsehood) ;
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
