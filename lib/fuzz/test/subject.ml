(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_fuzz
open Subject

let%test_module "program" =
  ( module struct
    open Program

    let%expect_test "empty program has no statements" =
      Fmt.(pr "%a@." (using has_statements bool) empty) ;
      [%expect {| false |}]
  end )

let%test_module "using sample environment" =
  ( module struct
    type r = Act_c_mini.Litmus.Lang.Program.t list Or_error.t
    [@@deriving sexp_of]

    let vars =
      Var.Map.make_existing_var_map
        (Lazy.force Act_c_mini_test.Env.test_env)
        Act_common.C_id.Set.empty

    let run programs =
      let result = Program.list_to_litmus programs ~vars in
      print_s [%sexp (result : r)]

    let%expect_test "programs_to_litmus: empty test" =
      run [] ; [%expect {| (Ok ()) |}]

    let%expect_test "programs_to_litmus: empty programs" =
      run (List.init 5 ~f:(fun _ -> Program.empty)) ;
      [%expect {| (Ok ()) |}]
  end )
