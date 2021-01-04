(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let%test_module "litmus_id" =
  ( module struct
    open Act_delitmus.Qualify

    let%expect_test "example with qualifying" =
      Fmt.pr "%a@." Act_common.C_id.pp
        (litmus_id (Act_common.Litmus_id.of_string "0:r0")) ;
      [%expect {| t0r0 |}]

    let%expect_test "example without qualifying" =
      Fmt.pr "%a@." Act_common.C_id.pp
        (litmus_id ~qualify_locals:false
           (Act_common.Litmus_id.of_string "0:r0")) ;
      [%expect {| r0 |}]
  end )
