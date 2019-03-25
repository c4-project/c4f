open Core

let has_extension ~ext file = snd (Filename.split_extension file) = Some ext

let%expect_test "has_extension: expected extension" =
  printf "%b" (has_extension ~ext:"h" "stdio.h");
  [%expect {| true |}]
;;

let%expect_test "has_extension: wrong extension" =
  printf "%b" (has_extension ~ext:"c" "stdio.h");
  [%expect {| false |}]
;;

let%expect_test "has_extension: no extension" =
  printf "%b" (has_extension ~ext:"h" "cstdio");
  [%expect {| false |}]
;;

let concat_list xs =
  xs |> List.reduce_balanced ~f:Filename.concat |> Option.value ~default:""
;;

let%expect_test "concat_list: empty list" =
  printf "%s" (concat_list []);
  [%expect {| |}]
;;

let%expect_test "concat_list: singleton" =
  printf "%s" (concat_list [ "foo" ]);
  [%expect {| foo |}]
;;

(* TODO(@MattWindsor91): work out how to do concat_list tests for
   2+ directories in a platform-independent manner. *)
