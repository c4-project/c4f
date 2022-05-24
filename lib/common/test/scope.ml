(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Src = C4f_common

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
