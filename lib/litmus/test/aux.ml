(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module A = Act_litmus.Aux
module Ac = Act_common

module J = A.Json (struct
  include Int
  let to_yojson i : Yojson.Safe.t = `Int i
  let of_yojson_exn : Yojson.Safe.t -> int = Yojson.Safe.Util.to_int
  let of_yojson (j : Yojson.Safe.t) : (int, string) Result.t =
    Result.try_with (fun () -> of_yojson_exn j)
    |> Result.map_error ~f:Exn.to_string

  let parse_post_string (s : string) : int Act_litmus.Ast_base.Postcondition.t Or_error.t =
    Or_error.try_with (fun () ->
        s |> Parsexp.Single.parse_string_exn |>
        [%of_sexp: int Act_litmus.Ast_base.Postcondition.t])
end)

let%test_module "JSON deserialisation" =
  ( module struct
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
      let init : (Ac.C_id.t, int) List.Assoc.t =
        [(x, 0); (y, 0)]
      in
      let postcondition : int Act_litmus.Ast_base.Postcondition.t =
        Act_litmus.Ast_base.
          { quantifier= `Exists
          ; predicate=
              Pred.(
                Elt Pred_elt.(a 0 ==? 0) && Elt Pred_elt.(a 1 ==? 1))
          }
      in
      let locations : Ac.C_id.t list = [x; y] in
      let aux = A.make ~init ~postcondition ~locations () in
      test aux ;
      [%expect
        {|
        {
          "locations": [ "x", "y" ],
          "init": { "x": 0, "y": 0 },
          "postcondition": "exists (0:a == 0 /\\ 1:a == 1)"
        } |}]
  end )

let%test_module "JSON serialisation" =
  ( module struct
    let test (json_str : string) : unit =
      let json = Yojson.Safe.from_string json_str in
      let aux = J.of_yojson_exn json in
      print_s [%sexp (aux : int A.t)]

    let%expect_test "SBSC example aux without a postcondition" =
      test {|
        {
          "locations": [ "x", "y" ],
          "init": { "x": 0, "y": 0 }
        }
        |};
      [%expect
        {| ((locations ((x y))) (init ((x 0) (y 0))) (postcondition ())) |}]
  end )

(* TODO(@MattWindsor91): test with postcondition *)
