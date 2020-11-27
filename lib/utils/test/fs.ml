(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_utils.Fs

let%test_module "filter_files" =
  ( module struct
    let test_files = ["stdio.h"; "conio.h"; "main.c"; "main.o"; "README"]

    let%expect_test "filter_files: no filter" =
      let result = filter_files (List.map ~f:Fpath.v test_files) in
      print_s [%sexp (List.map ~f:Fpath.to_string result : string list)] ;
      [%expect {| (stdio.h conio.h main.c main.o README) |}]

    let%expect_test "filter_files: filter" =
      let result = filter_files ~ext:"c" (List.map ~f:Fpath.v test_files) in
      print_s [%sexp (List.map ~f:Fpath.to_string result : string list)] ;
      [%expect {| (main.c) |}]
  end )

let%test_module "subpaths" =
  ( module struct
    let%expect_test "example absolute path" =
      Fmt.(
        pr "@[%a@]@." (list ~sep:sp Fpath.pp)
          (subpaths (Fpath.v "/usr/local/etc/blah/burble/baz"))) ;
      [%expect
        {|
      / /usr/ /usr/local/ /usr/local/etc/ /usr/local/etc/blah/
      /usr/local/etc/blah/burble/ /usr/local/etc/blah/burble/baz |}]

    let%expect_test "example relative path" =
      Fmt.(
        pr "@[%a@]@." (list ~sep:sp Fpath.pp)
          (subpaths (Fpath.v "../inky/pinky/parlez/vous"))) ;
      [%expect
        {| ../ ../inky/ ../inky/pinky/ ../inky/pinky/parlez/ ../inky/pinky/parlez/vous |}]
  end )
