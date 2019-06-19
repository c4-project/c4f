(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%test_module "with example map" =
  ( module struct
    module M = Act_delitmus.Var_map
    module Li = Act_common.Litmus_id
    module Ci = Act_common.C_id

    let map : M.t =
      M.of_map
        (Map.of_alist_exn
           (module Li)
           [ (Li.of_string "0:r0", Some (Ci.of_string "t0r0"))
           ; (Li.of_string "1:r0", Some (Ci.of_string "t1r0"))
           ; (Li.of_string "1:tmp", None)
           ; (Li.of_string "x", Some (Ci.of_string "x")) ])

    let%expect_test "global_c_variables" =
      Set.iter
        ~f:(fun x -> print_endline (Ci.to_string x))
        (M.global_c_variables map) ;
      [%expect {|
      t0r0
      t1r0
      x |}]
  end )
