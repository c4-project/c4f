(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Import

module Test_data = struct
  let existing_global : Fir.Type.t -> Src.Var.Record.t =
    Src.Var.Record.make_existing Common.Scope.Global

  let generated_global ?(initial_value : Fir.Constant.t option) :
      Fir.Type.t -> Src.Var.Record.t =
    Src.Var.Record.make_generated ?initial_value Common.Scope.Global

  let existing_local (tid : int) : Fir.Type.t -> Src.Var.Record.t =
    Src.Var.Record.make_existing (Common.Scope.Local tid)

  let generated_local ?(initial_value : Fir.Constant.t option) (tid : int) :
      Fir.Type.t -> Src.Var.Record.t =
    Src.Var.Record.make_generated ?initial_value (Local tid)

  let test_map : Src.Var.Map.t Lazy.t =
    lazy
      ( [ ("foo", existing_global (Fir.Type.int ()))
        ; ("bar", existing_global (Fir.Type.int ~is_atomic:true ()))
        ; ( "baz"
          , existing_global
              (Fir.Type.int ~is_atomic:true ~is_pointer:true ()) )
        ; ("foobar", existing_global (Fir.Type.bool ~is_atomic:true ()))
        ; ("barbaz", existing_global (Fir.Type.bool ()))
        ; ( "a"
          , generated_global ~initial_value:Fir.Constant.falsehood
              (Fir.Type.bool ()) )
        ; ( "b"
          , generated_global ~initial_value:Fir.Constant.truth
              (Fir.Type.bool ~is_atomic:true ()) )
        ; ("c", generated_global (Fir.Type.bool ()))
        ; ("d", existing_global (Fir.Type.int ()))
        ; ("e", generated_global (Fir.Type.int ()))
        ; ( "x"
          , generated_global ~initial_value:(Fir.Constant.int 27)
              (Fir.Type.int ~is_pointer:true ~is_atomic:true ()) )
        ; ( "y"
          , generated_global ~initial_value:(Fir.Constant.int 53)
              (Fir.Type.int ~is_pointer:true ~is_atomic:true ()) )
        ; ( "0:r0"
          , generated_local 0 ~initial_value:(Fir.Constant.int 4004)
              (Fir.Type.int ~is_atomic:true ()) )
        ; ( "0:r1"
          , generated_local 0 ~initial_value:(Fir.Constant.int 8008)
              (Fir.Type.int ()) )
        ; ("1:r0", existing_local 1 (Fir.Type.bool ()))
        ; ("1:r1", existing_local 1 (Fir.Type.int ()))
        ; ("2:r0", existing_local 2 (Fir.Type.int ()))
        ; ("2:r1", existing_local 2 (Fir.Type.bool ()))
        ; ("3:r0", existing_local 3 (Fir.Type.int ~is_pointer:true ())) ]
      |> Tx.Alist.map_left ~f:Common.Litmus_id.of_string
      |> Map.of_alist_exn (module Common.Litmus_id)
      |> Common.Scoped_map.of_litmus_id_map )
end

let%test_module "scopes_with_vars" =
  ( module struct
    let test (predicates : (Src.Var.Record.t -> bool) list) : unit =
      let scopes =
        Test_data.test_map |> Lazy.force
        |> Src.Var.Map.scopes_with_vars ~predicates
      in
      print_s [%sexp (scopes : Set.M(Common.Scope).t)]

    let%expect_test "all vars" =
      test [] ;
      [%expect {| ((Local 0) (Local 1) (Local 2) (Local 3) Global) |}]
  end )

let%test_module "gen_fresh_vars" =
  ( module struct
    let%test_unit "gen_fresh_vars generates unique fresh variables" =
      Base_quickcheck.Test.run_exn
        ( module struct
          type t = Common.C_id.t list [@@deriving sexp]

          let quickcheck_generator =
            Base_quickcheck.Generator.of_lazy
              Lazy.(Test_data.test_map >>| Src.Var.Map.gen_fresh_vars ~n:20)

          let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
        end )
        ~f:
          ([%test_pred: Common.C_id.t list] ~here:[[%here]]
             (Fn.non (List.contains_dup ~compare:[%compare: Common.C_id.t])) )
  end )

let%test_module "environment modules in a test map" =
  ( module struct
    let test_variables_of_basic_type (scope : Common.Scope.t)
        (basic : Fir.Type.Basic.t) : unit =
      let env =
        Src.Var.Map.env_satisfying_all
          (Lazy.force Test_data.test_map)
          ~scope ~predicates:[]
      in
      let vals =
        env
        |> C4f_fir.Env.variables_of_basic_type ~basic
        |> C4f_fir.Env.typing |> Map.to_alist
      in
      print_s [%sexp (vals : (Common.C_id.t, Fir.Type.t) List.Assoc.t)]

    let%expect_test "all integer variables from thread 1" =
      test_variables_of_basic_type (Common.Scope.Local 1)
        (Fir.Type.Basic.int ()) ;
      [%expect {| ((d int) (e int) (foo int) (r1 int)) |}]

    let%expect_test "all boolean variables from thread 2" =
      test_variables_of_basic_type (Common.Scope.Local 2)
        (Fir.Type.Basic.bool ()) ;
      [%expect {| ((a bool) (barbaz bool) (c bool) (r1 bool)) |}]

    let%expect_test "known values in environment form" =
      let env =
        Src.Var.Map.env_satisfying_all
          (Lazy.force Test_data.test_map)
          ~scope:Common.Scope.Global ~predicates:[]
      in
      let vals =
        env |> C4f_fir.Env.variables_with_known_values |> Map.to_alist
      in
      print_s
        [%sexp
          (vals : (Common.C_id.t, Fir.Type.t * Fir.Constant.t) List.Assoc.t)] ;
      [%expect
        {|
        ((a (bool (Bool false))) (b (atomic_bool (Bool true)))
         (x (atomic_int* (Int 27))) (y (atomic_int* (Int 53)))) |}]
  end )
