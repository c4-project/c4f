(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = C4f_litmus_c
end

let%test_module "sift_decls" =
  ( module struct
    let%expect_test "sift_decls: mixed example" =
      let result =
        Or_error.(
          [`Decl "foo"; `Decl "bar"; `Ndecl "baz"; `Ndecl "barbaz"]
          |> Src.Abstract_prim.sift_decls
          >>| fun (x, y) ->
          (x, List.map y ~f:(function `Decl _ -> "DECL" | `Ndecl x -> x)))
      in
      Stdio.print_s [%sexp (result : (string list * string list) Or_error.t)] ;
      [%expect {| (Ok ((foo bar) (baz barbaz))) |}]
  end )
