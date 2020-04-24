(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_c_mini
module Q = Base_quickcheck
module Qx = Act_utils.My_quickcheck

let variable_in (env : Src.Env.t) (l : Src.Address.t) : bool =
  Map.mem env (Src.Address.variable_of l)

let test_in_env (env : Src.Env.t)
    (module Qc : Qx.S_with_sexp with type t = Src.Address.t) : unit =
  Q.Test.run_exn
    (module Qc)
    ~f:([%test_pred: Src.Address.t] ~here:[[%here]] (variable_in env))

let test_type (env : Src.Env.t)
    (module Qc : Qx.S_with_sexp with type t = Src.Address.t)
    (expected : Src.Type.t) : unit =
  let module Tc = Src.Address.Type_check (struct
    let env = env
  end) in
  Q.Test.run_exn
    (module Qc)
    ~f:(fun lv ->
      [%test_result: Src.Type.t Or_error.t] ~here:[[%here]] (Tc.type_of lv)
        ~expect:(Or_error.return expected))

let%test_module "On_env" =
  ( module struct
    let e = Lazy.force Env.test_env

    module Qc = Src.Address_gen.On_env (struct
      let env = e
    end)

    let%expect_test "sample" =
      Qx.print_sample
        ( module struct
          include Src.Address
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
    let e = Lazy.force Env.test_env

    module Qc = Src.Address_gen.Atomic_int_pointers (struct
      let env = e
    end)

    let%expect_test "liveness" =
      Qx.print_sample
        ( module struct
          include Src.Address
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
      test_type e (module Qc) Src.Type.(int ~pointer:true ~atomic:true ())
  end )

let%test_module "Atomic_bool_pointers" =
  ( module struct
    let e = Lazy.force Env.test_env

    module Qc = Src.Address_gen.Atomic_bool_pointers (struct
      let env = e
    end)

    let%expect_test "Atomic_bool_pointers: liveness" =
      Qx.print_sample
        ( module struct
          include Src.Address
          include Qc
        end ) ;
      [%expect
        {|
      (Lvalue (Variable foobaz))
      (Ref (Lvalue (Variable z))) |}]

    let%test_unit "generated underlying variables in environment" =
      test_in_env e (module Qc)

    let%test_unit "generated lvalues have '*atomic_bool' type" =
      test_type e (module Qc) Src.Type.(bool ~pointer:true ~atomic:true ())
  end )
