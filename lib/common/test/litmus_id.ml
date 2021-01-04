(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = C4f_common
end

let%test_module "try_parse" =
  ( module struct
    let test (input : string) : unit =
      Stdio.print_s
        [%sexp (Src.Litmus_id.try_parse input : Src.Litmus_id.t Or_error.t)]

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
        (module Src.Litmus_id)
        ~f:(fun ident ->
          [%test_eq: Src.Litmus_id.t] ~here:[[%here]] ident
            Src.Litmus_id.(of_string (to_string ident)))

    let%test_unit "to_memalloy_id is identity on globals" =
      Base_quickcheck.Test.run_exn
        (module Src.C_id)
        ~f:(fun ident ->
          [%test_eq: Src.C_id.t] ~here:[[%here]] ident
            Src.Litmus_id.(to_memalloy_id (global ident)))
  end )

let%test_module "is_in_local_scope" =
  ( module struct
    let test (id : Src.Litmus_id.t) : unit =
      C4f_utils.Io.print_bool (Src.Litmus_id.is_in_local_scope id ~from:42)

    let%expect_test "global" =
      test (Src.Litmus_id.global (Src.C_id.of_string "foo")) ;
      [%expect {| true |}]

    let%expect_test "local: same thread ID" =
      test (Src.Litmus_id.local 42 (Src.C_id.of_string "foo")) ;
      [%expect {| true |}]

    let%expect_test "local: different thread ID" =
      test (Src.Litmus_id.local 9 (Src.C_id.of_string "foo")) ;
      [%expect {| false |}]
  end )

let%test_unit "to_memalloy_id doesn't throw when creating local identifiers"
    =
  Base_quickcheck.Test.run_exn
    ( module struct
      type t = C4f_utils.My_quickcheck.Small_non_negative_int.t * Src.C_id.t
      [@@deriving sexp, quickcheck]
    end )
    ~f:(fun (t, id) ->
      let mid = Src.Litmus_id.(to_memalloy_id (local t id)) in
      ignore (mid : Src.C_id.t))

let%test_module "Assoc" =
  ( module struct
    let%expect_test "try_parse: valid representative example" =
      let input = ["foo = barbaz"; "foobar"; "foo=bar=baz"] in
      let output =
        Src.Litmus_id.Assoc.try_parse input ~value_parser:(fun xo ->
            Ok (Option.value xo ~default:"[empty]"))
      in
      Stdio.print_s
        [%sexp (output : (Src.Litmus_id.t, string) List.Assoc.t Or_error.t)] ;
      [%expect {| (Ok ((foo barbaz) (foobar [empty]) (foo bar=baz))) |}]
  end )

let%test_module "is_in_scope" =
  ( module struct
    let test (id_str : string) (scope : Src.Scope.t) : unit =
      let id = Src.Litmus_id.of_string id_str in
      C4f_utils.Io.print_bool (Src.Litmus_id.is_in_scope id ~scope)

    let%expect_test "global in global scope" =
      test "foo" Src.Scope.Global ;
      [%expect {| true |}]

    let%expect_test "global in local scope" =
      test "foo" (Src.Scope.Local 0) ;
      [%expect {| true |}]

    let%expect_test "local in global scope" =
      test "0:foo" Src.Scope.Global ;
      [%expect {| false |}]

    let%expect_test "local in same local scope" =
      test "0:foo" (Src.Scope.Local 0) ;
      [%expect {| true |}]

    let%expect_test "local in different local scope" =
      test "0:foo" (Src.Scope.Local 1) ;
      [%expect {| false |}]
  end )
