(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Src = C4f_litmus_c

let%test_module "Bin" =
  ( module struct
    module Bin = Src.Operators.Bin

    let%test_module "binds_tighter" =
      ( module struct
        let test (this : Bin.t) (than : Bin.t) : unit =
          C4f_utils.Io.print_bool (Bin.binds_tighter this ~than)

        let%expect_test "equality binds tighter than comma" =
          test `Eq `Comma ; [%expect {| true |}]

        let%expect_test "bitwise OR binds tighter than logical OR" =
          test `Or `Lor ; [%expect {| true |}]

        let%expect_test "logical AND does not bind tighter than itself" =
          test `Land `Land ; [%expect {| false |}]
      end )
  end )
