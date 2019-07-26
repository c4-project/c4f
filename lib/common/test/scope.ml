(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Src = Act_common

let%test_module "is_in_scope" =
  ( module struct
    let test (id_str : string) (scope : Src.Scope.t) : unit =
      let id = Src.Litmus_id.of_string id_str in
      Act_utils.Io.print_bool (Src.Scope.id_in_scope ~id scope)

    let%expect_test "global in global scope" =
      test "foo" Src.Scope.Global ;
      [%expect {| true |}]

    let%expect_test "global in local scope" =
      test "foo" (Src.Scope.Local 0) ;
      [%expect {| true |}]

    let%expect_test "local in global scope" =
      test "0:foo" Src.Scope.Global ;
      [%expect {| false |}]

    let%expect_test "local in same local scope" =
      test "0:foo" (Src.Scope.Local 0) ;
      [%expect {| true |}]

    let%expect_test "local in different local scope" =
      test "0:foo" (Src.Scope.Local 1) ;
      [%expect {| false |}]
  end )

let%test_module "reduce" =
  ( module struct
    let test (l_scope : Src.Scope.t) (r_scope : Src.Scope.t) : unit =
      let l = (l_scope, "left") in
      let r = (r_scope, "right") in
      Stdio.print_endline (snd (Src.Scope.reduce l r))

    let%expect_test "both global scope" =
      Src.Scope.(test Global Global) ;
      [%expect {| left |}]

    let%expect_test "both local scope (same thread ID)" =
      Src.Scope.(test (Local 42) (Local 42)) ;
      [%expect {| left |}]

    (* Local scope with different thread IDs isn't well-defined, so we don't
       test it. *)

    let%expect_test "left global, right local" =
      Src.Scope.(test Global (Local 27)) ;
      [%expect {| right |}]

    let%expect_test "left local, right global" =
      Src.Scope.(test (Local 53) Global) ;
      [%expect {| left |}]
  end )
