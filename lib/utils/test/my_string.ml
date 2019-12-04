(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let%test_module "has_prefix" =
  ( module struct
    let test (str : string) (prefix : string) : unit =
      Fmt.pr "@[%b@]@." (Act_utils.My_string.has_prefix str ~prefix)

    let%expect_test "positive example" =
      test "spikemuth" "spike" ; [%expect {| true |}]

    let%expect_test "negative example" =
      test "enders" "east" ; [%expect {| false |}]

    let%expect_test "empty example" =
      test "barracuda" "" ; [%expect {| true |}]
  end )

let%test_module "ensure_prefix" =
  ( module struct
    let test (str : string) (prefix : string) : unit =
      Fmt.pr "@[%s@]@." (Act_utils.My_string.ensure_prefix str ~prefix)

    let%expect_test "positive example" =
      test "spikemuth" "spike" ; [%expect {| spikemuth |}]

    let%expect_test "negative example" =
      test "enders" "east" ; [%expect {| eastenders |}]

    let%expect_test "empty example" =
      test "barracuda" "" ; [%expect {| barracuda |}]
  end )

let%test_module "has_suffix" =
  ( module struct
    let test (str : string) (suffix : string) : unit =
      Fmt.pr "@[%b@]@." (Act_utils.My_string.has_suffix str ~suffix)

    let%expect_test "positive example" =
      test "Lewisham" "ham" ; [%expect {| true |}]

    let%expect_test "negative example" =
      test "Bexley" "heath" ; [%expect {| false |}]

    let%expect_test "empty example" =
      test "Tower Hamlets" "" ; [%expect {| true |}]
  end )

let%test_module "ensure_suffix" =
  ( module struct
    let test (str : string) (suffix : string) : unit =
      Fmt.pr "@[%s@]@." (Act_utils.My_string.ensure_suffix str ~suffix)

    let%expect_test "positive example" =
      test "Lewisham" "ham" ; [%expect {| Lewisham |}]

    let%expect_test "negative example" =
      test "Bexley" "heath" ; [%expect {| Bexleyheath |}]

    let%expect_test "empty example" =
      test "Tower Hamlets" "" ; [%expect {| Tower Hamlets |}]
  end )
