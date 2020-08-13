(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fuzz_run
  module F = Act_fuzz
end

let%test_module "default param map" =
  ( module struct
    let conf : Src.Config.t = Src.Config.make ()

    let map : F.Param_map.t = Src.Config.make_param_map conf

    let pp_param : int Or_error.t Fmt.t =
      Fmt.(result ~error:Error.pp ~ok:Int.pp)

    let%expect_test "action cap is present" =
      Fmt.pr "@[%a@]@." pp_param (F.Param_map.get_action_cap map) ;
      [%expect {| 40 |}]

    let%expect_test "thread cap is present" =
      Fmt.pr "@[%a@]@." pp_param (F.Param_map.get_thread_cap map) ;
      [%expect {| 16 |}]
  end )
