(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_c_mini.Lvalue
module Q = Base_quickcheck

let%expect_test "variable_in_env: positive variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env (variable (Act_common.C_id.of_string "foo"))
        : bool )] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env (variable (Act_common.C_id.of_string "kappa"))
        : bool )] ;
  [%expect {| false |}]

let%expect_test "variable_in_env: positive deref result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          (deref (variable (Act_common.C_id.of_string "bar")))
        : bool )] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          (deref (variable (Act_common.C_id.of_string "keepo")))
        : bool )] ;
  [%expect {| false |}]

let%expect_test "Type-checking a valid normal variable lvalue" =
  let module T = Type_check ((val Lazy.force Env.test_env_mod)) in
  let result = T.type_of (variable (Act_common.C_id.of_string "foo")) in
  print_s [%sexp (result : Act_c_mini.Type.t Or_error.t)] ;
  [%expect {| (Ok int) |}]

let%expect_test "Type-checking an invalid deferencing variable lvalue" =
  let module T = Type_check ((val Lazy.force Env.test_env_mod)) in
  let result =
    T.type_of (deref (variable (Act_common.C_id.of_string "foo")))
  in
  print_s [%sexp (result : Act_c_mini.Type.t Or_error.t)] ;
  [%expect {| (Error "not a pointer type") |}]

let%expect_test "gen: sample" =
  Act_utils.My_quickcheck.print_sample (module Act_c_mini.Lvalue) ;
  [%expect
    {|
    (Variable J___r8___Ps_____rS_K)
    (Variable P_ZvJK_s)
    (Variable QC___PG)
    (Variable Snh2_IR___EjA_wx)
    (Variable ZI_9__8z7__j__)
    (Variable _CR_)
    (Variable __)
    (Variable _gAO__o)
    (Variable n_P_)
    (Variable o73Y15D)
    (Variable t)
    (Deref (Variable Al))
    (Deref (Variable MV___q_r))
    (Deref (Variable _))
    (Deref (Variable _S______z1_73ts_8))
    (Deref (Variable __D))
    (Deref (Deref (Variable __u_E7_fn)))
    (Deref (Deref (Deref (Deref (Variable b__v))))) |}]

let%test_unit "on_value_of_typed_id: always takes basic type" =
  let (module E) = Lazy.force Env.test_env_mod in
  let module Tc = Type_check (E) in
  Q.Test.run_exn
    (module E.Random_var)
    ~f:(fun id ->
      let ty = Map.find_exn E.env id in
      [%test_result: Act_c_mini.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_value_of_typed_id ~id ~ty))
        ~expect:(Or_error.return Act_c_mini.Type.(normal (basic_type ty))))
