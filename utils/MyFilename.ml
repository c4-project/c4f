open Core

let has_extension ~ext file =
  snd (Filename.split_extension file) = Some ext

let%expect_test "has_extension: expected extension" =
  printf "%b" (has_extension ~ext:"h" "stdio.h");
  [%expect {| true |}]

let%expect_test "has_extension: wrong extension" =
  printf "%b" (has_extension ~ext:"c" "stdio.h");
  [%expect {| false |}]

let%expect_test "has_extension: no extension" =
  printf "%b" (has_extension ~ext:"h" "cstdio");
  [%expect {| false |}]
