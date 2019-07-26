(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_common

let%test_module "on sample map" =
  ( module struct
    let map : string Src.Scoped_map.t =
      Src.Litmus_id.
        [ (global (Src.C_id.of_string "foo"), "a foo")
        ; (global (Src.C_id.of_string "bar"), "a bar")
        ; (global (Src.C_id.of_string "baz"), "a baz")
        ; (local 0 (Src.C_id.of_string "avocado"), "some avocado")
        ; (local 0 (Src.C_id.of_string "eggplant"), "some eggplant")
        ; (local 0 (Src.C_id.of_string "bar"), "some bar")
        ; (local 1 (Src.C_id.of_string "xyzzy"), "the xyzzy")
        ; (local 1 (Src.C_id.of_string "plugh"), "the plugh")
        ; (local 1 (Src.C_id.of_string "baz"), "the baz") ]
      |> Map.of_alist_exn (module Src.Litmus_id)
      |> Src.Scoped_map.of_litmus_id_map

    let%test_module "c_id_mem" =
      ( module struct
        let test (id_str : string) : unit =
          let id = Src.C_id.of_string id_str in
          Act_utils.Io.print_bool (Src.Scoped_map.c_id_mem map ~id)

        let%expect_test "valid non-shadowed global" =
          test "foo" ; [%expect {| true |}]

        let%expect_test "valid shadowed global" =
          test "bar" ; [%expect {| true |}]

        let%expect_test "valid non-shadowing local" =
          test "xyzzy" ; [%expect {| true |}]

        let%expect_test "invalid" = test "burble" ; [%expect {| false |}]
      end )

    let%test_module "resolve" =
      ( module struct
        let test (id_str : string) (scope : Src.Scope.t) : unit =
          map
          |> Src.Scoped_map.resolve ~id:(Src.C_id.of_string id_str) ~scope
          |> Src.Litmus_id.to_string |> Stdio.print_endline

        let%expect_test "non-shadowing local in local scope" =
          test "avocado" (Src.Scope.Local 0) ;
          [%expect {| 0:avocado |}]

        let%expect_test "shadowing local in local scope" =
          test "bar" (Src.Scope.Local 0) ;
          [%expect {| 0:bar |}]

        let%expect_test "non-shadowed global in local scope" =
          test "foo" (Src.Scope.Local 1) ;
          [%expect {| foo |}]

        let%expect_test "shadowed global in non-shadowing local scope" =
          test "bar" (Src.Scope.Local 1) ;
          [%expect {| bar |}]

        let%expect_test "non-shadowed global in global scope" =
          test "foo" Src.Scope.Global ;
          [%expect {| foo |}]

        let%expect_test "shadowed global in global scope" =
          test "baz" Src.Scope.Global ;
          [%expect {| baz |}]
      end )

    let%test_module "to_c_id_map" =
      ( module struct
        let test (scope : Src.Scope.t) : unit =
          map
          |> Src.Scoped_map.to_c_id_map ~scope
          |> Map.to_alist
          |> Fmt.(
               pr "@[<v>%a@]@."
                 (list (box (pair ~sep:comma Src.C_id.pp string))))

        let%expect_test "global scope" =
          test Src.Scope.Global ;
          [%expect
            {|
       bar, a bar
       baz, a baz
       foo, a foo |}]

        let%expect_test "local scope 0" =
          test (Src.Scope.Local 0) ;
          [%expect
            {|
       avocado, some avocado
       bar, some bar
       baz, a baz
       eggplant, some eggplant
       foo, a foo |}]

        let%expect_test "local scope 1" =
          test (Src.Scope.Local 1) ;
          [%expect
            {|
       bar, a bar
       baz, the baz
       foo, a foo
       plugh, the plugh
       xyzzy, the xyzzy |}]

        let%expect_test "local scope 2 (deliberately unpopulated)" =
          test (Src.Scope.Local 2) ;
          [%expect
            {|
       bar, a bar
       baz, a baz
       foo, a foo |}]
      end )
  end )
