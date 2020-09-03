(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open struct
  module Src = Act_fir
end

let%test_module "convert" = (module struct
  let test (x : Src.Constant.t) (to_ : Src.Type.Prim.t) : unit =
    Stdio.print_s [%sexp (Src.Constant.convert x ~to_ : Src.Constant.t Or_error.t)]

  let%expect_test "truthy int to bool" =
    test (Src.Constant.int 27) Bool;
    [%expect {| (Ok (Bool true)) |}]

  let%expect_test "falsy int to bool" =
    test (Src.Constant.int 0) Bool;
    [%expect {| (Ok (Bool false)) |}]

  let%expect_test "truth to int" =
    test (Src.Constant.truth) Int;
    [%expect {| (Ok (Int 1)) |}]

  let%expect_test "falsehood to int" =
    test (Src.Constant.falsehood) Int;
    [%expect {| (Ok (Int 0)) |}]


end)
