(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module A = Act_common
module Src = Act_state
module Tx = Travesty_base_exts

module Test_utils = struct
  let entry_exn (xs : (string, string) List.Assoc.t) : Src.Entry.t =
    xs
    |> Tx.Alist.map_left ~f:A.Litmus_id.of_string
    |> Src.Entry.of_alist |> Or_error.ok_exn

  let entries_exn (input : (string, string) List.Assoc.t list) :
      Set.M(Src.Entry).t =
    input
    |> List.map ~f:(Tx.Alist.map_left ~f:Act_common.Litmus_id.of_string)
    |> Tx.Or_error.combine_map ~f:Src.Entry.of_alist
    |> Or_error.ok_exn
    |> Set.of_list (module Src.Entry)
end

let%test_module "common_domain" =
  ( module struct
    let test (states : Src.Entry.t list) : unit =
      print_s
        [%sexp
          (Src.Entry.common_domain states : Set.M(A.Litmus_id).t Or_error.t)]

    let%expect_test "no states" = test [] ; [%expect {| (Ok ()) |}]

    let test_state : Src.Entry.t =
      Test_utils.entry_exn
        [("0:foo", "snap"); ("1:bar", "crackle"); ("baz", "pop")]

    let%expect_test "single state" =
      test [test_state] ;
      [%expect {| (Ok (baz 0:foo 1:bar)) |}]

    let%expect_test "consistent domains" =
      test
        [ test_state
        ; Test_utils.entry_exn
            [("0:foo", "power"); ("1:bar", "courage"); ("baz", "wisdom")]
        ; Test_utils.entry_exn
            [ ("0:foo", "hamburger")
            ; ("1:bar", "cheeseburger")
            ; ("baz", "veggieburger") ] ] ;
      [%expect {| (Ok (baz 0:foo 1:bar)) |}]

    let%expect_test "inconsistent domains" =
      test
        [ test_state
        ; Test_utils.entry_exn [("0:foo", "power"); ("baz", "wisdom")]
        ; Test_utils.entry_exn
            [("1:bar", "cheeseburger"); ("baz", "veggieburger")] ] ;
      [%expect
        {|
        (Error
         (("Domains of states are inconsistent: for example,"
           (one_domain (baz 0:foo 1:bar)) (another_domain (baz 0:foo)))
          ("Domains of states are inconsistent: for example,"
           (one_domain (baz 0:foo 1:bar)) (another_domain (baz 1:bar))))) |}]
  end )
