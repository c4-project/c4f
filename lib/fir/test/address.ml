(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck
open Stdio
open Act_fir.Address

let%test_module "normalise" =
  ( module struct
    let%test_unit "normalise is idempotent" =
      Base_quickcheck.Test.run_exn
        (module Act_fir.Address)
        ~f:(fun addr ->
          [%test_result: Act_fir.Address.t] ~here:[[%here]]
            ~equal:[%equal: Act_fir.Address.t]
            Act_fir.Address.(normalise (normalise addr))
            ~expect:(normalise addr))
  end )

let%test_module "variable_of" =
  ( module struct
    let%test_unit "variable_of: preserved by ref" =
      Q.Test.run_exn
        (module Act_fir.Address)
        ~f:(fun x ->
          [%test_eq: Act_common.C_id.t] ~here:[[%here]] (variable_of x)
            (variable_of (ref x)))

    let%expect_test "variable_of: nested example" =
      let example =
        ref
          (ref
             (lvalue
                Act_fir.(
                  Lvalue.deref
                    (Lvalue.variable (Act_common.C_id.of_string "yorick")))))
      in
      let var = variable_of example in
      Fmt.pr "%a@." Act_common.C_id.pp var ;
      [%expect {| yorick |}]
  end )

let%test_module "Type-check" =
  ( module struct
    module T = Type_check (struct
      let env = Lazy.force Env.test_env
    end)

    let test (addr : t) : unit =
      let result = T.type_of addr in
      print_s [%sexp (result : Act_fir.Type.t Or_error.t)]

    let%expect_test "Type-checking a valid normal variable lvalue" =
      test (of_variable (Act_common.C_id.of_string "foo")) ;
      [%expect {| (Ok int) |}]

    let%expect_test "Type-checking an valid reference lvalue" =
      test (of_variable_ref (Act_common.C_id.of_string "foo")) ;
      [%expect {| (Ok int*) |}]
  end )

let%test_module "deref" =
  ( module struct
    let%test_unit "deref of ref is equivalent to normalise" =
      Base_quickcheck.Test.run_exn
        (module Act_fir.Address)
        ~f:(fun addr ->
          [%test_result: Act_fir.Address.t] ~here:[[%here]]
            ~equal:[%equal: Act_fir.Address.t]
            Act_fir.Address.(deref (ref addr))
            ~expect:(normalise addr))
  end )

let%test_unit "on_address_of_typed_id: always takes pointer type" =
  let env = Lazy.force Env.test_env in
  let module Tc = Type_check (struct
    let env = env
  end) in
  Base_quickcheck.Test.run_exn
    ( module Act_fir.Env.Random_var_with_type (struct
      let env = env
    end) )
    ~f:(fun r ->
      let ty = Accessor.(r.@(Act_common.C_named.value)) in
      [%test_result: Act_fir.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_address_of_typed_id r))
        ~expect:
          (Or_error.return
             Act_fir.Type.(make ~is_pointer:true (basic_type ty))))
