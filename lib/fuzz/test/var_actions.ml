(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = Act_fuzz

let%test_module "Make_global" =
  ( module struct
    let%test_module "Payload" =
      ( module struct
        let%test_unit "constant is the right type" =
          Test.run_exn
            ( module struct
              type t = Src.Var_actions.Global_payload.t [@@deriving sexp]

              let quickcheck_generator =
                Src.Var_actions.Global_payload.generator
                  (Lazy.force Var.Test_data.test_map)

              let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
            end )
            ~f:
              (fun Src.Var_actions.Global_payload.
                     {basic_type; initial_value; _} ->
              let _ =
                Or_error.ok_exn
                  Act_c_mini.(
                    Type.check_modulo_atomicity
                      (Type.of_basic basic_type)
                      (Constant.type_of initial_value))
              in
              ())
      end )
  end )
