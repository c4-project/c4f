(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let%test_module "format_for_readme" =
  ( module struct
    let test (str : string) : unit =
      Fmt.pr "@[%s@]@." (Act_utils.My_string.format_for_readme str)

    let%expect_test "empty" = test "" ; [%expect {| |}]

    let%expect_test "short example" =
      test "here is a short example" ;
      [%expect {| here is a short example |}]

    let%expect_test "long example" =
      test
        {|
        This is hopefully a representative example of an unformatted
        README.
        It has
        line breaks in very strange places.
        

        Here is another paragraph. |} ;
      [%expect
        {|
          This is hopefully a representative example of an unformatted README. It has
          line breaks in very strange places.

          Here is another paragraph. |}]
  end )
