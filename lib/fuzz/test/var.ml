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

let%test_module "environment modules in a test map" =
  ( module struct
    let existing_global : Ty.t -> Src.Var.Record.t =
      Src.Var.Record.make_existing Ac.Scope.Global

    let existing_local (tid : int) : Ty.t -> Src.Var.Record.t =
      Src.Var.Record.make_existing (Ac.Scope.Local tid)

    let test_map : Src.Var.Map.t =
      [ ("foo", existing_global (Ty.int ()))
      ; ("bar", existing_global (Ty.int ~atomic:true ()))
      ; ("baz", existing_global (Ty.int ~atomic:true ~pointer:true ()))
      ; ("foobar", existing_global (Ty.bool ~atomic:true ()))
      ; ("barbaz", existing_global (Ty.bool ()))
      ; ( "a"
        , Src.Var.Record.make_generated_global ~initial_value:Con.falsehood
            (Ty.bool ()) )
      ; ( "b"
        , Src.Var.Record.make_generated_global ~initial_value:Con.truth
            (Ty.bool ~atomic:true ()) )
      ; ("c", Src.Var.Record.make_generated_global (Ty.bool ()))
      ; ( "d"
        , Src.Var.Record.make_generated_global ~initial_value:(Con.int 27)
            (Ty.int ()) )
      ; ( "e"
        , Src.Var.Record.make_generated_global ~initial_value:(Con.int 53)
            (Ty.int ~atomic:true ()) )
      ; ("f", Src.Var.Record.make_generated_global (Ty.int ()))
      ; ("1:x", existing_local 1 (Ty.bool ()))
      ; ("1:y", existing_local 1 (Ty.int ()))
      ; ("2:x", existing_local 1 (Ty.int ()))
      ; ("2:y", existing_local 1 (Ty.bool ()))
      ; ("3:x", existing_local 1 (Ty.int ~pointer:true ())) ]
      |> Tx.Alist.map_left ~f:Ac.Litmus_id.of_string
      |> Map.of_alist_exn (module Ac.Litmus_id)
      |> Ac.Scoped_map.of_litmus_id_map

    let test_variables_of_basic_type (scope : Ac.Scope.t) (ty : Ty.Basic.t) :
        unit =
      let (module Env) =
        Src.Var.Map.env_module_with_known_values test_map ~scope
      in
      let vals = ty |> Env.variables_of_basic_type |> Map.to_alist in
      print_s [%sexp (vals : (Ac.C_id.t, Ty.t) List.Assoc.t)]

    let%expect_test "all integer variables from thread 1" =
      test_variables_of_basic_type (Ac.Scope.Local 1) (Ty.Basic.int ()) ;
      [%expect {| ((d int) (f int) (foo int) (y int)) |}]

    let%expect_test "all boolean variables from thread 2" =
      test_variables_of_basic_type (Ac.Scope.Local 2) (Ty.Basic.bool ()) ;
      [%expect {| ((a bool) (barbaz bool) (c bool) (y bool)) |}]

    let%expect_test "known values in environment form" =
      let (module Env) =
        Src.Var.Map.env_module_with_known_values test_map
          ~scope:Ac.Scope.Global
      in
      let vals =
        Env.variables_with_known_values |> Lazy.force |> Map.to_alist
      in
      print_s [%sexp (vals : (Ac.C_id.t, Ty.t * Con.t) List.Assoc.t)] ;
      [%expect
        {|
        ((a (bool (Bool false))) (b (atomic_bool (Bool true))) (d (int (Int 27)))
         (e (atomic_int (Int 53)))) |}]
  end )
