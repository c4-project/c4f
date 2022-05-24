(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let variable_in (env : Fir.Env.t) (l : Fir.Lvalue.t) : bool =
  Map.mem env l.@(Fir.Lvalue.variable_of)

let print_sample = Utils.My_quickcheck.print_sample

let test_in_env (env : Fir.Env.t)
    (module Qc : Utils.My_quickcheck.S_with_sexp with type t = Fir.Lvalue.t)
    : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Fir.Lvalue.t] ~here:[[%here]] (variable_in env))

let test_type (env : Fir.Env.t)
    (module Qc : Utils.My_quickcheck.S_with_sexp with type t = Fir.Lvalue.t)
    (expected : Fir.Type.t) : unit =
  let module Tc = Fir.Lvalue.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun lv ->
      [%test_result: Fir.Type.t Or_error.t]
        ~here:[[%here]]
        (Tc.type_of lv) ~expect:(Ok expected) )

let%test_module "On_env" =
  ( module struct
    let%expect_test "sample" =
      let e = Lazy.force Fir_test.Env.test_env in
      let module Qc = Src.Lvalue.On_env (struct
        let env = e
      end) in
      print_sample
        ( module struct
          include Fir.Lvalue
          include Qc
        end ) ;
      [%expect
        {|
        (Variable bar)
        (Variable barbaz)
        (Variable foo)
        (Variable x)
        (Variable z)
        (Deref (Variable bar))
        (Deref (Variable barbaz))
        (Deref (Variable x))
        (Deref (Variable y))
        (Deref (Variable z))
        (Deref (Deref (Variable foo)))
        (Deref (Deref (Variable y)))
        (Deref (Deref (Deref (Variable barbaz))))
        (Deref (Deref (Deref (Variable x))))
        (Deref (Deref (Deref (Variable z))))
        (Deref (Deref (Deref (Deref (Deref (Variable bar)))))) |}]

    let%test_unit "generated underlying variables in environment" =
      let e = Lazy.force Fir_test.Env.test_env in
      test_in_env e
        ( module Src.Lvalue.On_env (struct
          let env = e
        end) )
  end )

let%test_module "Int_values" =
  ( module struct
    let e = Lazy.force Fir_test.Env.test_env

    module Qc = Src.Lvalue.Int_values (struct
      let env = e
    end)

    let print_sample (e : Fir.Env.t) =
      print_sample
        ( module struct
          include Fir.Lvalue

          include Src.Lvalue.Int_values (struct
            let env = e
          end)
        end )

    let%expect_test "sample" =
      print_sample e ;
      [%expect
        {|
          (Variable foo)
          (Deref (Variable blep)) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Qc)

    let%test_unit "generated lvalues have 'int' type" =
      test_type e (module Qc) Fir.Type.(int ())
  end )

let%test_module "Bool_values" =
  ( module struct
    let e = Lazy.force Fir_test.Env.test_env

    let print_sample (e : Fir.Env.t) =
      print_sample
        ( module struct
          include Fir.Lvalue

          include Src.Lvalue.Bool_values (struct
            let env = e
          end)
        end )

    let%expect_test "sample" =
      print_sample e ; [%expect {| (Variable barbaz) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e
        ( module Src.Lvalue.Bool_values (struct
          let env = e
        end) )

    let%test_unit "generated lvalues have 'bool' type" =
      test_type e
        ( module Src.Lvalue.Bool_values (struct
          let env = e
        end) )
        Fir.Type.(bool ())
  end )
