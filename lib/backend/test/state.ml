(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module A = Act_common
module Src = Act_backend
module Tx = Travesty_base_exts

let%test_module "common_domain" =
  ( module struct
    let test (states : Src.State.t list) : unit =
      print_s
        [%sexp
          (Src.State.common_domain states : Set.M(A.Litmus_id).t Or_error.t)]

    let state_exn (xs : (string, string) List.Assoc.t) : Src.State.t =
      xs
      |> Tx.Alist.map_left ~f:A.Litmus_id.of_string
      |> Src.State.of_alist |> Or_error.ok_exn

    let%expect_test "no states" = test [] ; [%expect {| (Ok ()) |}]

    let test_state : Src.State.t =
      state_exn [("0:foo", "snap"); ("1:bar", "crackle"); ("baz", "pop")]

    let%expect_test "single state" =
      test [test_state] ;
      [%expect {| (Ok (baz 0:foo 1:bar)) |}]

    let%expect_test "consistent domains" =
      test
        [ test_state
        ; state_exn
            [("0:foo", "power"); ("1:bar", "courage"); ("baz", "wisdom")]
        ; state_exn
            [ ("0:foo", "hamburger")
            ; ("1:bar", "cheeseburger")
            ; ("baz", "veggieburger") ] ] ;
      [%expect {| (Ok (baz 0:foo 1:bar)) |}]

    let%expect_test "inconsistent domains" =
      test
        [ test_state
        ; state_exn [("0:foo", "power"); ("baz", "wisdom")]
        ; state_exn [("1:bar", "cheeseburger"); ("baz", "veggieburger")] ] ;
      [%expect
        {|
        (Error
         (("Domains of states are inconsistent: for example,"
           (one_domain (baz 0:foo 1:bar)) (another_domain (baz 0:foo)))
          ("Domains of states are inconsistent: for example,"
           (one_domain (baz 0:foo 1:bar)) (another_domain (baz 1:bar))))) |}]
  end )
