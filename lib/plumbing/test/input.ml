(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Input = Plumbing.Input

let test : Input.t -> unit = Fmt.pr "@[%a@]@." Input.pp

let test_err : Input.t Or_error.t -> unit =
  Fmt.(pr "@[%a@]@." (result ~ok:Input.pp ~error:Error.pp))

let%test_module "of_fpath" =
  ( module struct
    let%expect_test "paths map to files" =
      test (Input.of_fpath Fpath.(v "Foo" / "bar" / "baz.barbaz")) ;
      [%expect {| Foo/bar/baz.barbaz |}]
  end )

let%test_module "of_fpath_opt" =
  ( module struct
    let%expect_test "None maps to stdin" =
      test (Input.of_fpath_opt None) ;
      [%expect {| (stdin) |}]

    let%expect_test "paths map to files" =
      test (Input.of_fpath_opt (Some Fpath.(v "Foo" / "bar" / "baz.barbaz"))) ;
      [%expect {| Foo/bar/baz.barbaz |}]
  end )

let%test_module "of_string_opt" =
  ( module struct
    let%expect_test "None maps to stdin" =
      test_err (Input.of_string_opt None) ;
      [%expect {| (stdin) |}]

    let%expect_test "valid paths map to files" =
      test_err (Input.of_string_opt (Some "Foo/bar/baz.barbaz")) ;
      [%expect {| Foo/bar/baz.barbaz |}]

    let%expect_test "invalid paths map to errors" =
      test_err (Input.of_string_opt (Some "")) ;
      [%expect {| "": invalid path |}]
  end )

let%test_module "of_string" =
  ( module struct
    let%expect_test "'-' maps to stdin" =
      test_err (Input.of_string "-") ;
      [%expect {| (stdin) |}]

    let%expect_test "valid paths map to files" =
      test_err (Input.of_string "Foo/bar/baz.barbaz") ;
      [%expect {| Foo/bar/baz.barbaz |}]

    let%expect_test "invalid paths map to errors" =
      test_err (Input.of_string "") ;
      [%expect {| "": invalid path |}]
  end )
