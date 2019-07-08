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
open Act_c_mini.Address

let%test_module "variable_of" =
  ( module struct
    let%test_unit "variable_of: preserved by ref" =
      Q.Test.run_exn
        (module Act_c_mini.Address)
        ~f:(fun x ->
          [%test_eq: Act_common.C_id.t] ~here:[[%here]] (variable_of x)
            (variable_of (ref x)))

    let%expect_test "variable_of: nested example" =
      let example =
        ref
          (ref
             (lvalue
                Act_c_mini.(
                  Lvalue.deref
                    (Lvalue.variable
                       (Act_common.C_id.of_string "yorick")))))
      in
      let var = variable_of example in
      Fmt.pr "%a@." Act_common.C_id.pp var ;
      [%expect {| yorick |}]
  end )

let%test_module "Type-check" =
  ( module struct
    module T = Type_check ((val Lazy.force Act_c_mini.Env.test_env_mod))

    let test (addr : t) : unit =
      let result = T.type_of addr in
      print_s [%sexp (result : Act_c_mini.Type.t Or_error.t)]

    let%expect_test "Type-checking a valid normal variable lvalue" =
      test (of_variable (Act_common.C_id.of_string "foo")) ;
      [%expect {| (Ok (Normal int)) |}]

    let%expect_test "Type-checking an valid reference lvalue" =
      test (of_variable_ref (Act_common.C_id.of_string "foo")) ;
      [%expect {| (Ok (Pointer_to int)) |}]
  end )

let%test_unit "on_address_of_typed_id: always takes pointer type" =
  let (module E) = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Tc = Type_check (E) in
  Base_quickcheck.Test.run_exn
    (module E.Random_var)
    ~f:(fun id ->
      let ty = Act_common.C_id.Map.find_exn E.env id in
      [%test_result: Act_c_mini.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_address_of_typed_id ~id ~ty))
        ~expect:
          (Or_error.return Act_c_mini.Type.(pointer_to (basic_type ty))))

let variable_in (module E : Act_c_mini.Env_types.S) (l : t) : bool =
  Act_common.C_id.Map.mem E.env (variable_of l)

let%expect_test "Quickcheck_on_env: liveness" =
  let e = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Qc = Quickcheck_on_env ((val e)) in
  Q.Test.with_sample_exn [%quickcheck.generator: Qc.t]
    ~config:{ Base_quickcheck.Test.default_config with test_count = 20 }
    ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.dedup_and_sort ~compare:[%compare: Act_c_mini.Address.t]
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s
      );
	[%expect {|
  (Lvalue (Variable bar))
  (Lvalue (Variable barbaz))
  (Lvalue (Variable blep))
  (Lvalue (Variable foo))
  (Lvalue (Variable y))
  (Lvalue (Deref (Variable foo)))
  (Lvalue (Deref (Deref (Variable foo))))
  (Lvalue (Deref (Deref (Deref (Deref (Deref (Deref (Variable x))))))))
  (Ref (Lvalue (Variable barbaz)))
  (Ref (Lvalue (Variable x)))
  (Ref (Lvalue (Deref (Variable x))))
  (Ref (Lvalue (Deref (Deref (Variable y)))))
  (Ref (Lvalue (Deref (Deref (Deref (Deref (Variable bar)))))))
  (Ref (Ref (Lvalue (Variable foo))))
  (Ref (Ref (Lvalue (Variable y))))
  (Ref (Ref (Lvalue (Deref (Deref (Variable x))))))
  (Ref (Ref (Lvalue (Deref (Deref (Deref (Variable barbaz)))))))
  (Ref (Ref (Ref (Lvalue (Deref (Variable blep))))))
  (Ref (Ref (Ref (Lvalue (Deref (Variable y)))))) |}]

let%test_unit "Quickcheck_on_env: generated underlying variables in \
               environment" =
  let e = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Q = Quickcheck_on_env ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in e))

let%expect_test "Quickcheck_atomic_int_pointers: liveness" =
  let e = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Qc = Quickcheck_atomic_int_pointers ((val e)) in
  Q.Test.with_sample_exn [%quickcheck.generator: Qc.t]
    ~config:{ Base_quickcheck.Test.default_config with test_count = 20 }
    ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.dedup_and_sort ~compare:[%compare: Act_c_mini.Address.t]
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s
      );
  [%expect {|
    (Lvalue (Variable bar))
    (Ref (Lvalue (Variable x)))
    (Ref (Lvalue (Variable y))) |}]

let%test_unit "Quickcheck_atomic_int_pointers: generated underlying \
               variables in environment" =
  let e = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Q = Quickcheck_atomic_int_pointers ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in e))

let%test_unit "Quickcheck_int_values: generated lvalues have '*atomic_int' \
               type" =
  let e = Lazy.force Act_c_mini.Env.test_env_mod in
  let module Q = Quickcheck_atomic_int_pointers ((val e)) in
  let module Tc = Type_check ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:(fun lv ->
      [%test_result: Act_c_mini.Type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of lv)
        ~expect:
          (Or_error.return Act_c_mini.Type.(pointer_to Basic.atomic_int)))
