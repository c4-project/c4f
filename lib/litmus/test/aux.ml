(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let%test_module "JSON deserialisation" =
  ( module struct
    module A = Act_litmus.Aux
    module Ac = Act_common

    module J = A.Json (struct
      include Int

      let to_yojson (i : t) : Yojson.Safe.t = `Int i
      let of_yojson_exn = Yojson.Safe.Util.to_int
      let of_yojson (j : Yojson.Safe.t) : (t, string) Result.t =
        Result.try_with (fun () -> of_yojson_exn j)
        |> Result.map_error ~f:(Exn.to_string)

      let parse_post_string _ = Or_error.unimplemented "parse_post_string"
    end)

    type t = int A.t

    let test (aux : t) : unit =
      let json = J.to_yojson aux in
      Yojson.Safe.pretty_to_channel ~std:true stdout json

    let str_local (tid : int) (str : string) : Ac.Litmus_id.t =
      Ac.Litmus_id.local tid (Ac.C_id.of_string str)

    let%expect_test "empty aux" =
      test (A.make ()) ;
      [%expect
        {| { "locations": null, "init": {}, "postcondition": null } |}]

    let%expect_test "SBSC example aux" =
      let a (tid : int) = str_local tid "a" in
      let x = Ac.C_id.of_string "x" in
      let y = Ac.C_id.of_string "y" in
      let init : (Ac.C_id.t, int) List.Assoc.t = [(x, 0); (y, 0)] in
      let postcondition : int Act_litmus.Ast_base.Postcondition.t =
        Act_litmus.Ast_base.
          { quantifier= `Exists
          ; predicate=
              Pred.(Elt Pred_elt.(a 0 ==? 0) && Elt Pred_elt.(a 1 ==? 0)) }
      in
      let locations : Ac.C_id.t list = [x; y] in
      let aux = A.make ~init ~postcondition ~locations () in
      test aux ;
      [%expect
        {|
        {
          "locations": [ "x", "y" ],
          "init": { "x": 0, "y": 0 },
          "postcondition": "exists (0:a == 0 /\\ 1:a == 0)"
        } |}]
  end )
