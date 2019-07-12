(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_c_mini
open Lvalue_gen
module Q = Base_quickcheck

let variable_in (module E : Env_types.S) (l : Lvalue.t) : bool =
  Act_common.C_id.Map.mem E.env (Lvalue.variable_of l)

let print_sample = Act_utils.My_quickcheck.print_sample

let test_in_env (module E : Env_types.S)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp with type t = Lvalue.t)
    : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Lvalue.t] ~here:[[%here]] (variable_in (module E)))

let test_type (module E : Act_c_mini.Env_types.S)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp
      with type t = Act_c_mini.Lvalue.t) (expected : Act_c_mini.Type.t) :
    unit =
  let module Tc = Lvalue.Type_check (E) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun lv ->
      [%test_result: Act_c_mini.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of lv)
        ~expect:(Or_error.return expected))

let%test_module "On_env" =
  ( module struct
    let%expect_test "sample" =
      let e = Lazy.force Act_c_mini.Env.test_env_mod in
      let module Qc = On_env ((val e)) in
      print_sample
        ( module struct
          include Act_c_mini.Lvalue
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
      let e = Lazy.force Act_c_mini.Env.test_env_mod in
      test_in_env e (module On_env ((val e)))
  end )

let%test_module "Int_values" =
  ( module struct
    let e = Lazy.force Act_c_mini.Env.test_env_mod

    let print_sample (module E : Act_c_mini.Env_types.S) =
      print_sample
        ( module struct
          include Act_c_mini.Lvalue
          include Int_values (E)
        end )

    let%expect_test "sample" =
      print_sample e ;
      [%expect
        {|
          (Variable foo)
          (Deref (Variable blep)) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Int_values ((val e)))

    let%test_unit "generated lvalues have 'int' type" =
      test_type e
        (module Int_values ((val e)))
        Act_c_mini.Type.(normal Basic.int)
  end )

let%test_module "Bool_values" =
  ( module struct
    let e = Lazy.force Act_c_mini.Env.test_env_mod

    let print_sample (module E : Act_c_mini.Env_types.S) =
      print_sample
        ( module struct
          include Act_c_mini.Lvalue
          include Bool_values (E)
        end )

    let%expect_test "sample" =
      print_sample e ; [%expect {| (Variable barbaz) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Bool_values ((val e)))

    let%test_unit "generated lvalues have 'bool' type" =
      test_type e
        (module Bool_values ((val e)))
        Act_c_mini.Type.(normal Basic.bool)
  end )
