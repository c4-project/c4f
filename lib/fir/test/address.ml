(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck
open Stdio
open C4f_fir.Address

let%test_module "normalise" =
  ( module struct
    let%test_unit "normalise is idempotent" =
      Base_quickcheck.Test.run_exn
        (module C4f_fir.Address)
        ~f:(fun addr ->
          [%test_result: C4f_fir.Address.t]
            ~here:[[%here]]
            ~equal:[%equal: C4f_fir.Address.t]
            C4f_fir.Address.(normalise (normalise addr))
            ~expect:(normalise addr) )
  end )

let%test_module "variable_of" =
  ( module struct
    let%test_unit "variable_of: preserved by ref" =
      Q.Test.run_exn
        (module C4f_fir.Address)
        ~f:(fun x ->
          [%test_eq: C4f_common.C_id.t]
            ~here:[[%here]]
            (Accessor.get variable_of x)
            (Accessor.get variable_of (Ref x)) )

    let%expect_test "variable_of: nested example" =
      let example =
        Ref
          (Ref
             (Lvalue
                C4f_fir.(
                  Lvalue.(
                    Accessor.construct deref (of_variable_str_exn "yorick") ) )
             ) )
      in
      let var = Accessor.get variable_of example in
      Fmt.pr "%a@." C4f_common.C_id.pp var ;
      [%expect {| yorick |}]
  end )

let%test_module "Type-check" =
  ( module struct
    module T = Type_check (struct
      let env = Lazy.force Env.test_env
    end)

    let test (addr : t) : unit =
      let result = T.type_of addr in
      print_s [%sexp (result : C4f_fir.Type.t Or_error.t)]

    let%expect_test "Type-checking a valid normal variable lvalue" =
      test (Accessor.construct variable (C4f_common.C_id.of_string "foo")) ;
      [%expect {| (Ok int) |}]

    let%expect_test "Type-checking an valid reference lvalue" =
      test
        (Accessor.construct variable_ref (C4f_common.C_id.of_string "foo")) ;
      [%expect {| (Ok int*) |}]
  end )

let%test_module "deref" =
  ( module struct
    let%test_unit "deref of ref is equivalent to normalise" =
      Base_quickcheck.Test.run_exn
        (module C4f_fir.Address)
        ~f:(fun addr ->
          [%test_result: C4f_fir.Address.t]
            ~here:[[%here]]
            ~equal:[%equal: C4f_fir.Address.t]
            C4f_fir.Address.(deref (Ref addr))
            ~expect:(normalise addr) )
  end )

let%test_unit "on_address_of_typed_id: always takes pointer type" =
  let env = Lazy.force Env.test_env in
  let module Tc = Type_check (struct
    let env = env
  end) in
  Base_quickcheck.Test.run_exn
    ( module C4f_fir.Env.Random_var_with_type (struct
      let env = env
    end) )
    ~f:(fun r ->
      let ty = Accessor.(r.@(C4f_common.C_named.value)) in
      [%test_result: C4f_fir.Type.t Or_error.t]
        ~here:[[%here]]
        (Tc.type_of (on_address_of_typed_id r))
        ~expect:
          (Or_error.return
             C4f_fir.Type.(make ~is_pointer:true (basic_type ty)) ) )
