(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_state
module Tx = Travesty_base_exts

let%test_module "convert_states" =
  ( module struct
    let test (input : (string, string) List.Assoc.t list) : unit =
      let entries =
        input
        |> List.map ~f:(Tx.Alist.map_left ~f:Act_common.Litmus_id.of_string)
        |> Tx.Or_error.combine_map ~f:Src.Entry.of_alist
        |> Or_error.ok_exn
      in
      let output =
        Src.Dnf.convert_states (Set.of_list (module Src.Entry) entries)
      in
      Fmt.pr "@[%a@]@."
        (Act_litmus.Postcondition.pp ~pp_const:String.pp)
        output

    let%expect_test "empty set" = test [] ; [%expect {| forall (true) |}]

    let%expect_test "set of empty observation" =
      test [[]] ;
      [%expect {| forall (true) |}]

    let%expect_test "sample set" =
      let raw_entries =
        [ [("foo", "1"); ("0:bar", "2"); ("1:baz", "3")]
        ; [("foo", "1"); ("0:bar", "4"); ("1:baz", "9")]
        ; [("foo", "1"); ("0:bar", "10")]
        ; [] ]
      in
      test raw_entries ; [%expect {| forall (true) |}]
  end )
