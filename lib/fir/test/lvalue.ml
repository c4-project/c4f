(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open C4f_fir.Lvalue
module Q = Base_quickcheck

let variable_in_env (x : t) ~(env : C4f_fir.Env.t) : bool =
  Map.mem env Accessor.(x.@(variable_of))

let%expect_test "variable_in_env: positive variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          (Accessor.construct variable (C4f_common.C_id.of_string "foo"))
        : bool )] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          (Accessor.construct variable (C4f_common.C_id.of_string "kappa"))
        : bool )] ;
  [%expect {| false |}]

let%expect_test "variable_in_env: positive deref result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          Accessor.(
            construct (deref @> variable) (C4f_common.C_id.of_string "bar"))
        : bool )] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Env.test_env in
  print_s
    [%sexp
      ( variable_in_env ~env
          Accessor.(
            construct (deref @> variable) (C4f_common.C_id.of_string "keepo"))
        : bool )] ;
  [%expect {| false |}]

let%expect_test "Type-checking a valid normal variable lvalue" =
  let module T = Type_check (struct
    let env = Lazy.force Env.test_env
  end) in
  let result = T.type_of (of_variable_str_exn "foo") in
  print_s [%sexp (result : C4f_fir.Type.t Or_error.t)] ;
  [%expect {| (Ok int) |}]

let%expect_test "Type-checking an invalid deferencing variable lvalue" =
  let module T = Type_check (struct
    let env = Lazy.force Env.test_env
  end) in
  let result =
    T.type_of
      Accessor.(
        construct (deref @> variable) (C4f_common.C_id.of_string "foo"))
  in
  print_s [%sexp (result : C4f_fir.Type.t Or_error.t)] ;
  [%expect
    {|
    (Error
     ("While checking underlying type of lvalue dereferencing:" (Variable foo)
      ("tried to get value type of a non-pointer type" (ty.basic_type int)))) |}]

let%expect_test "gen: sample" =
  C4f_utils.My_quickcheck.print_sample (module C4f_fir.Lvalue) ;
  [%expect
    {|
    (Variable C7s_n)
    (Variable JUATvMOOQ_5__79toi4TX)
    (Variable L)
    (Variable R_l7x1_DtR)
    (Variable ZxjS_I0A_UPr)
    (Variable aGxObBDD)
    (Variable j)
    (Variable sqg__ZTYwX)
    (Deref (Variable N_snbpe))
    (Deref (Variable O))
    (Deref (Variable _ftnD7))
    (Deref (Variable i_SRwKe0))
    (Deref (Variable nfqe))
    (Deref (Variable wbYpbRVz1N3_St))
    (Deref (Deref (Variable Lo)))
    (Deref (Deref (Variable cGPmS_oQ9l_wsKX)))
    (Deref (Deref (Variable pJw5IlP_HsShx__)))
    (Deref (Deref (Deref (Variable bg1nh2XjLEjA))))
    (Deref (Deref (Deref (Deref (Variable H)))))
    (Deref (Deref (Deref (Deref (Deref (Variable _9lCUCr6)))))) |}]

let%test_unit "on_value_of_typed_id: always takes basic type" =
  let env = Lazy.force Env.test_env in
  let module Tc = Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    ( module C4f_fir.Env.Random_var_with_type (struct
      let env = env
    end) )
    ~f:(fun r ->
      let ty = Accessor.(r.@(C4f_common.C_named.value)) in
      [%test_result: C4f_fir.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_value_of_typed_id r))
        ~expect:(Or_error.return C4f_fir.Type.(make (basic_type ty))) )
