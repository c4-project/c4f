(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_c_mini
open Address_gen
module Q = Base_quickcheck

let variable_in (module E : Env_types.S) (l : Address.t) : bool =
  Act_common.C_id.Map.mem E.env (Address.variable_of l)

let print_sample = Act_utils.My_quickcheck.print_sample

let test_in_env (module E : Env_types.S)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp
      with type t = Act_c_mini.Address.t) : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Address.t] ~here:[[%here]] (variable_in (module E)))

let test_type (module E : Env_types.S)
    (module Qc : Act_utils.My_quickcheck.S_with_sexp with type t = Address.t)
    (expected : Type.t) : unit =
  let module Tc = Address.Type_check (E) in
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
          include Act_c_mini.Address
          include Qc
        end ) ;
      [%expect
        {|
  (Lvalue (Variable bar))
  (Lvalue (Variable barbaz))
  (Lvalue (Variable blep))
  (Lvalue (Variable x))
  (Lvalue (Deref (Variable foo)))
  (Lvalue (Deref (Deref (Variable barbaz))))
  (Lvalue (Deref (Deref (Deref (Deref (Deref (Deref (Variable bar))))))))
  (Ref (Lvalue (Variable blep)))
  (Ref (Lvalue (Variable z)))
  (Ref (Lvalue (Deref (Variable bar))))
  (Ref (Lvalue (Deref (Deref (Variable x)))))
  (Ref (Lvalue (Deref (Deref (Deref (Deref (Variable blep)))))))
  (Ref (Ref (Lvalue (Variable foo))))
  (Ref (Ref (Lvalue (Variable z))))
  (Ref (Ref (Lvalue (Deref (Deref (Variable bar))))))
  (Ref (Ref (Lvalue (Deref (Deref (Deref (Variable z)))))))
  (Ref (Ref (Ref (Lvalue (Deref (Variable x))))))
  (Ref (Ref (Ref (Lvalue (Deref (Variable y)))))) |}]

    let%test_unit "generated underlying variables in environment" =
      let e = Lazy.force Act_c_mini.Env.test_env_mod in
      test_in_env e (module On_env ((val e)))
  end )

let%test_module "Atomic_int_pointers" =
  ( module struct
    let e = Lazy.force Act_c_mini.Env.test_env_mod

    let%expect_test "liveness" =
      let module Qc = Atomic_int_pointers ((val e)) in
      print_sample
        ( module struct
          include Act_c_mini.Address
          include Qc
        end ) ;
      [%expect
        {|
    (Lvalue (Variable bar))
    (Ref (Lvalue (Variable x)))
    (Ref (Lvalue (Variable y))) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Atomic_int_pointers ((val e)))

    let%test_unit "generated lvalues have '*atomic_int' type" =
      test_type e
        (module Atomic_int_pointers ((val e)))
        Act_c_mini.Type.(pointer_to Basic.atomic_int)
  end )

let%test_module "Atomic_bool_pointers" =
  ( module struct
    let e = Lazy.force Act_c_mini.Env.test_env_mod

    let%expect_test "Atomic_bool_pointers: liveness" =
      let module Qc = Atomic_bool_pointers ((val e)) in
      print_sample
        ( module struct
          include Act_c_mini.Address
          include Qc
        end ) ;
      [%expect
        {|
      (Lvalue (Variable foobaz))
      (Ref (Lvalue (Variable z))) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Atomic_bool_pointers ((val e)))

    let%test_unit "generated lvalues have '*atomic_bool' type" =
      test_type e
        (module Atomic_bool_pointers ((val e)))
        Act_c_mini.Type.(pointer_to Basic.atomic_bool)
  end )
