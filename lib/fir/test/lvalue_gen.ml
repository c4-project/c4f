(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck
module Src = Act_fir

let variable_in (env : Src.Env.t) (l : Src.Lvalue.t) : bool =
  Map.mem env (Src.Lvalue.variable_of l)

let print_sample = Act_utils.My_quickcheck.print_sample

let test_in_env (env : Src.Env.t)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp
      with type t = Src.Lvalue.t) : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Src.Lvalue.t] ~here:[[%here]] (variable_in env))

let test_type (env : Src.Env.t)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp
      with type t = Src.Lvalue.t) (expected : Src.Type.t) : unit =
  let module Tc = Src.Lvalue.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun lv ->
      [%test_result: Src.Type.t Or_error.t] ~here:[[%here]] (Tc.type_of lv)
        ~expect:(Or_error.return expected))

let%test_module "On_env" =
  ( module struct
    let%expect_test "sample" =
      let e = Lazy.force Env.test_env in
      let module Qc = Src.Lvalue_gen.On_env (struct
        let env = e
      end) in
      print_sample
        ( module struct
          include Src.Lvalue
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
      let e = Lazy.force Env.test_env in
      test_in_env e
        ( module Src.Lvalue_gen.On_env (struct
          let env = e
        end) )
  end )

let%test_module "Int_values" =
  ( module struct
    let e = Lazy.force Env.test_env

    module Qc = Src.Lvalue_gen.Int_values (struct
      let env = e
    end)

    let print_sample (e : Src.Env.t) =
      print_sample
        ( module struct
          include Src.Lvalue

          include Src.Lvalue_gen.Int_values (struct
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
      test_type e (module Qc) Src.Type.(int ())
  end )

let%test_module "Bool_values" =
  ( module struct
    let e = Lazy.force Env.test_env

    let print_sample (e : Src.Env.t) =
      print_sample
        ( module struct
          include Src.Lvalue

          include Src.Lvalue_gen.Bool_values (struct
            let env = e
          end)
        end )

    let%expect_test "sample" =
      print_sample e ; [%expect {| (Variable barbaz) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e
        ( module Src.Lvalue_gen.Bool_values (struct
          let env = e
        end) )

    let%test_unit "generated lvalues have 'bool' type" =
      test_type e
        ( module Src.Lvalue_gen.Bool_values (struct
          let env = e
        end) )
        Src.Type.(bool ())
  end )
