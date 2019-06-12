(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let%test_module "local" = (module struct
  open Act_delitmus.Qualify

  let%expect_test "example" =
    Fmt.pr "%a@." Act_common.C_id.pp (local 0 (Act_common.C_id.of_string "r0")) ;
    [%expect {| t0r0 |}]
end)
