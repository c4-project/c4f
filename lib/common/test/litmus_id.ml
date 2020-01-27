(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

let%test_module "try_parse" =
  ( module struct
    let test (input : string) : unit =
      Stdio.print_s
        [%sexp (Ac.Litmus_id.try_parse input : Ac.Litmus_id.t Or_error.t)]

    let%expect_test "example local identifier" =
      test "0:r1" ; [%expect {| (Ok 0:r1) |}]

    let%expect_test "example global identifier" =
      test "x" ; [%expect {| (Ok x) |}]

    let%expect_test "example invalid identifier" =
      test "0:1" ;
      [%expect
        {|
    (Error
     ("validation failed"
      (1
       ("validation errors" (("initial char '1'" "Invalid initial character.")))
       lib/common/src/c_id.ml:49:13))) |}]
  end )

let%test_module "Id tests" =
  ( module struct
    let%test_unit "to_string->of_string is identity" =
      Base_quickcheck.Test.run_exn
        (module Ac.Litmus_id)
        ~f:(fun ident ->
          [%test_eq: Ac.Litmus_id.t] ~here:[[%here]] ident
            Ac.Litmus_id.(of_string (to_string ident)))

    let%test_unit "to_memalloy_id is identity on globals" =
      Base_quickcheck.Test.run_exn
        (module Ac.C_id)
        ~f:(fun ident ->
          [%test_eq: Ac.C_id.t] ~here:[[%here]] ident
            Ac.Litmus_id.(to_memalloy_id (global ident)))
  end )

let%test_module "is_in_scope" =
  ( module struct
    let test (id : Ac.Litmus_id.t) : unit =
      Act_utils.Io.print_bool (Ac.Litmus_id.is_in_scope id ~from:42)

    let%expect_test "global" =
      test (Ac.Litmus_id.global (Ac.C_id.of_string "foo")) ;
      [%expect {| true |}]

    let%expect_test "local: same thread ID" =
      test (Ac.Litmus_id.local 42 (Ac.C_id.of_string "foo")) ;
      [%expect {| true |}]

    let%expect_test "local: different thread ID" =
      test (Ac.Litmus_id.local 9 (Ac.C_id.of_string "foo")) ;
      [%expect {| false |}]
  end )

let%test_unit "to_memalloy_id doesn't throw when creating local identifiers"
    =
  Base_quickcheck.Test.run_exn
    ( module struct
      type t = Act_utils.My_quickcheck.Small_non_negative_int.t * Ac.C_id.t
      [@@deriving sexp, quickcheck]
    end )
    ~f:(fun (t, id) ->
      let mid = Ac.Litmus_id.(to_memalloy_id (local t id)) in
      ignore (mid : Ac.C_id.t))

let%test_module "Assoc" =
  ( module struct
    let%expect_test "try_parse: valid representative example" =
      let input = ["foo = barbaz"; "foobar"; "foo=bar=baz"] in
      let output =
        Ac.Litmus_id.Assoc.try_parse input ~value_parser:(fun xo ->
            Ok (Option.value xo ~default:"[empty]"))
      in
      Stdio.print_s
        [%sexp (output : (Ac.Litmus_id.t, string) List.Assoc.t Or_error.t)] ;
      [%expect {| (Ok ((foo barbaz) (foobar [empty]) (foo bar=baz))) |}]
  end )
