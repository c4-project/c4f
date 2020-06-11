(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Src = Act_litmus_c

let%test_module "Bin" =
  ( module struct
    module Bin = Src.Operators.Bin

    let%test_module "binds_tighter" =
      ( module struct
        let test (this : Bin.t) (than : Bin.t) : unit =
          Act_utils.Io.print_bool (Bin.binds_tighter this ~than)

        let%expect_test "equality binds tighter than comma" =
          test `Eq `Comma ;
          [%expect {| true |}]

        let%expect_test "bitwise OR binds tighter than logical OR" =
          test `Or `Lor ;
          [%expect {| true |}]

        let%expect_test "logical AND does not bind tighter than itself" =
          test `Land `Land ;
          [%expect {| false |}]
      end )
  end )
