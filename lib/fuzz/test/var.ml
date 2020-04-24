(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_fuzz
module Ac = Act_common
module Con = Act_c_mini.Constant
module Ty = Act_c_mini.Type
module Tx = Travesty_base_exts

module Test_data = struct
  let existing_global : Ty.t -> Src.Var.Record.t =
    Src.Var.Record.make_existing Ac.Scope.Global

  let generated_global ?(initial_value : Con.t option) :
      Ty.t -> Src.Var.Record.t =
    Src.Var.Record.make_generated ?initial_value Ac.Scope.Global

  let existing_local (tid : int) : Ty.t -> Src.Var.Record.t =
    Src.Var.Record.make_existing (Ac.Scope.Local tid)

  let test_map : Src.Var.Map.t Lazy.t =
    lazy
      ( [ ("foo", existing_global (Ty.int ()))
        ; ("bar", existing_global (Ty.int ~atomic:true ()))
        ; ("baz", existing_global (Ty.int ~atomic:true ~pointer:true ()))
        ; ("foobar", existing_global (Ty.bool ~atomic:true ()))
        ; ("barbaz", existing_global (Ty.bool ()))
        ; ("a", generated_global ~initial_value:Con.falsehood (Ty.bool ()))
        ; ( "b"
          , generated_global ~initial_value:Con.truth
              (Ty.bool ~atomic:true ()) )
        ; ("c", generated_global (Ty.bool ()))
        ; ("d", existing_global (Ty.int ()))
        ; ("e", generated_global (Ty.int ()))
        ; ( "x"
          , generated_global ~initial_value:(Con.int 27)
              (Ty.int ~atomic:true ()) )
        ; ( "y"
          , generated_global ~initial_value:(Con.int 53)
              (Ty.int ~atomic:true ()) )
        ; ("1:r0", existing_local 1 (Ty.bool ()))
        ; ("1:r1", existing_local 1 (Ty.int ()))
        ; ("2:r0", existing_local 2 (Ty.int ()))
        ; ("2:r1", existing_local 2 (Ty.bool ()))
        ; ("3:r0", existing_local 3 (Ty.int ~pointer:true ())) ]
      |> Tx.Alist.map_left ~f:Ac.Litmus_id.of_string
      |> Map.of_alist_exn (module Ac.Litmus_id)
      |> Ac.Scoped_map.of_litmus_id_map )
end

let%test_module "environment modules in a test map" =
  ( module struct
    let test_variables_of_basic_type (scope : Ac.Scope.t)
        (basic : Ty.Basic.t) : unit =
      let env =
        Src.Var.Map.env_satisfying_all
          (Lazy.force Test_data.test_map)
          ~scope ~predicates:[]
      in
      let vals =
        env
        |> Act_c_mini.Env.variables_of_basic_type ~basic
        |> Act_c_mini.Env.typing |> Map.to_alist
      in
      print_s [%sexp (vals : (Ac.C_id.t, Ty.t) List.Assoc.t)]

    let%expect_test "all integer variables from thread 1" =
      test_variables_of_basic_type (Ac.Scope.Local 1) (Ty.Basic.int ()) ;
      [%expect {| ((d int) (e int) (foo int) (r1 int)) |}]

    let%expect_test "all boolean variables from thread 2" =
      test_variables_of_basic_type (Ac.Scope.Local 2) (Ty.Basic.bool ()) ;
      [%expect {| ((a bool) (barbaz bool) (c bool) (r1 bool)) |}]

    let%expect_test "known values in environment form" =
      let env =
        Src.Var.Map.env_satisfying_all
          (Lazy.force Test_data.test_map)
          ~scope:Ac.Scope.Global ~predicates:[]
      in
      let vals =
        env |> Act_c_mini.Env.variables_with_known_values |> Map.to_alist
      in
      print_s [%sexp (vals : (Ac.C_id.t, Ty.t * Con.t) List.Assoc.t)] ;
      [%expect
        {|
        ((a (bool (Bool false))) (b (atomic_bool (Bool true)))
         (x (atomic_int (Int 27))) (y (atomic_int (Int 53)))) |}]
  end )
