(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let variable_in (env : Fir.Env.t) (l : Fir.Address.t) : bool =
  (* TODO(@MattWindsor91) Is this duplicated? *)
  Map.mem env l.@(Fir.Address.variable_of)

let test_in_env (env : Fir.Env.t)
    (module Qc : Utils.My_quickcheck.S_with_sexp with type t = Fir.Address.t)
    : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Fir.Address.t] ~here:[[%here]] (variable_in env))

let test_type (env : Fir.Env.t)
    (module Qc : Utils.My_quickcheck.S_with_sexp with type t = Fir.Address.t)
    (expected : Fir.Type.t) : unit =
  let module Tc = Fir.Address.Type_check (struct
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
    let e = Lazy.force Fir_test.Env.test_env

    module Qc = Src.Address.On_env (struct
      let env = e
    end)

    let%expect_test "sample" =
      Utils.My_quickcheck.print_sample
        ( module struct
          include Fir.Address
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
      test_in_env e (module Qc)
  end )

let%test_module "Atomic_int_pointers" =
  ( module struct
    let e = Lazy.force Fir_test.Env.test_env

    module Qc = Src.Address.Atomic_int_pointers (struct
      let env = e
    end)

    let%expect_test "liveness" =
      Utils.My_quickcheck.print_sample
        ( module struct
          include Fir.Address
          include Qc
        end ) ;
      [%expect
        {|
    (Lvalue (Variable bar))
    (Ref (Lvalue (Variable x)))
    (Ref (Lvalue (Variable y))) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Qc)

    let%test_unit "generated lvalues have '*atomic_int' type" =
      test_type e
        (module Qc)
        Fir.Type.(int ~is_pointer:true ~is_atomic:true ())
  end )

let%test_module "Atomic_bool_pointers" =
  ( module struct
    let e = Lazy.force Fir_test.Env.test_env

    module Qc = Src.Address.Atomic_bool_pointers (struct
      let env = e
    end)

    let%expect_test "Atomic_bool_pointers: liveness" =
      Utils.My_quickcheck.print_sample
        ( module struct
          include Fir.Address
          include Qc
        end ) ;
      [%expect
        {|
      (Lvalue (Variable foobaz))
      (Ref (Lvalue (Variable z))) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Qc)

    let%test_unit "generated lvalues have '*atomic_bool' type" =
      test_type e
        (module Qc)
        Fir.Type.(bool ~is_pointer:true ~is_atomic:true ())
  end )
