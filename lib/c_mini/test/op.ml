(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_c_mini
  module Q = Base_quickcheck
end

let%test_module "algebraic properties" =
  ( module struct
    let eval_int_op (op : Src.Op.Binary.t) (l : int) (r : int) :
        int Or_error.t =
      let expr = Src.Expression.(bop op (int_lit l) (int_lit r)) in
      Or_error.(
        expr
        |> Src.Expression_eval.as_constant ~env:(Src.Heap.empty ())
        >>= Src.Constant.as_int)

    let test_int_result got ~expect ~here =
      [%test_result: int Or_error.t] got ~expect:(Ok expect) ~here:[here]

    let%test_module "arith" =
      ( module struct
        let gen_arith (f : Src.Op.Binary.Arith.t -> bool) :
            (module Q.Test.S with type t = Src.Op.Binary.Arith.t * int) =
          ( module struct
            open Q

            module A = struct
              type t = Src.Op.Binary.Arith.t [@@deriving sexp, quickcheck]

              let quickcheck_generator =
                Generator.filter quickcheck_generator ~f
            end

            type t = A.t * int [@@deriving sexp, quickcheck]
          end )

        let%test_unit "zero_lhs_unit" =
          Q.Test.run_exn (gen_arith Src.Op.Binary.Arith.zero_lhs_unit)
            ~f:(fun (op, rhs) ->
              test_int_result
                (eval_int_op (Arith op) 0 rhs)
                ~expect:rhs ~here:[%here])

        let%test_unit "zero_rhs_unit" =
          Q.Test.run_exn (gen_arith Src.Op.Binary.Arith.zero_rhs_unit)
            ~f:(fun (op, lhs) ->
              test_int_result
                (eval_int_op (Arith op) lhs 0)
                ~expect:lhs ~here:[%here])

        let%test_unit "refl_zero" =
          Q.Test.run_exn (gen_arith Src.Op.Binary.Arith.refl_zero)
            ~f:(fun (op, r) ->
              test_int_result
                (eval_int_op (Arith op) r r)
                ~expect:0 ~here:[%here])
      end )

    let%test_module "bitwise" =
      ( module struct
        let gen_bitwise (f : Src.Op.Binary.Bitwise.t -> bool) :
            (module Q.Test.S with type t = Src.Op.Binary.Bitwise.t * int) =
          ( module struct
            open Q

            module A = struct
              type t = Src.Op.Binary.Bitwise.t [@@deriving sexp, quickcheck]

              let quickcheck_generator =
                Generator.filter quickcheck_generator ~f
            end

            type t = A.t * int [@@deriving sexp, quickcheck]
          end )

        let%test_unit "zero_lhs_unit" =
          Q.Test.run_exn (gen_bitwise Src.Op.Binary.Bitwise.zero_lhs_unit)
            ~f:(fun (op, rhs) ->
              test_int_result
                (eval_int_op (Bitwise op) 0 rhs)
                ~expect:rhs ~here:[%here])

        let%test_unit "zero_rhs_unit" =
          Q.Test.run_exn (gen_bitwise Src.Op.Binary.Bitwise.zero_rhs_unit)
            ~f:(fun (op, lhs) ->
              test_int_result
                (eval_int_op (Bitwise op) lhs 0)
                ~expect:lhs ~here:[%here])

        let%test_unit "refl_zero" =
          Q.Test.run_exn (gen_bitwise Src.Op.Binary.Bitwise.refl_zero)
            ~f:(fun (op, r) ->
              test_int_result
                (eval_int_op (Bitwise op) r r)
                ~expect:0 ~here:[%here])
      end )
  end )
