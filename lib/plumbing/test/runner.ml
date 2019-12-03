(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Plumbing
end

let%test_module "program functions" =
  ( module struct
    let test_with (prog : string)
        (input : Src.Copy_projection.t Src.Copy_spec.t)
        ~(f : Src.Runner_types.prog_fun) : unit =
      Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:string) (f prog ~input))

    let proj (local : string) (remote : string) : Src.Copy_projection.t =
      Src.Copy_projection.make ~local:(Fpath.v local) ~remote

    let%test_module "id_prog" =
      ( module struct
        let test : string -> Src.Copy_projection.t Src.Copy_spec.t -> unit =
          test_with ~f:Src.Runner.id_prog

        let%expect_test "empty spec" =
          test "sl" Src.Copy_spec.nothing ;
          [%expect {| sl |}]

        let%expect_test "directory spec" =
          test "cat" (Src.Copy_spec.directory (proj "bleh" "blah")) ;
          [%expect {| cat |}]

        let%expect_test "single file spec, not the program" =
          test "dog" (Src.Copy_spec.file (proj "bleh" "blah")) ;
          [%expect {| dog |}]

        let%expect_test "single file spec, being the program" =
          test "dog" (Src.Copy_spec.file (proj "dog" "quinoa")) ;
          [%expect {| dog |}]
      end )

    let%test_module "copy_prog" =
      ( module struct
        let test : string -> Src.Copy_projection.t Src.Copy_spec.t -> unit =
          test_with ~f:Src.Runner.copy_prog

        let%expect_test "empty spec" =
          test "sl" Src.Copy_spec.nothing ;
          [%expect {| sl |}]

        let%expect_test "directory spec" =
          test "cat" (Src.Copy_spec.directory (proj "bleh" "blah")) ;
          [%expect {| cat |}]

        let%expect_test "single file spec, not the program" =
          test "dog" (Src.Copy_spec.file (proj "bleh" "blah")) ;
          [%expect {| dog |}]

        let%expect_test "single file spec, being the program" =
          test "dog" (Src.Copy_spec.file (proj "dog" "quinoa")) ;
          [%expect {| quinoa |}]
      end )
  end )
