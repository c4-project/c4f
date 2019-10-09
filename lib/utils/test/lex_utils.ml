(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let read_string_from_string (s : string) : string =
  Act_utils.Lex_utils.read_string Fn.id
    (Sedlexing.Utf8.from_string (s ^ "\""))

let%test_module "escape_string" =
  ( module struct
    let test (s : string) : unit =
      Stdio.print_endline (Act_utils.Lex_utils.escape_string s)

    let%expect_test "empty string" = test "" ; [%expect {||}]

    let%expect_test "string with no escaping necessary" =
      test "precision German engineering" ;
      [%expect {| precision German engineering |}]

    let%expect_test "string with quotes" =
      test {| "Leonard Bernstein!" |} ;
      [%expect {| \"Leonard Bernstein!\" |}]

    let%expect_test "string with existing escapes" =
      test {| \" |} ; [%expect {| \\\" |}]
  end )

let%test_module "read_string" =
  ( module struct
    let test (s : string) : unit =
      Stdio.print_endline (read_string_from_string s)

    let%expect_test "empty string" = test "" ; [%expect {||}]

    let%expect_test "string with no escaping necessary" =
      test "precision German engineering" ;
      [%expect {| precision German engineering |}]

    let%expect_test "string with escaped quotes" =
      test {| \"Leonard Bernstein!\" |} ;
      [%expect {| "Leonard Bernstein!" |}]

    let%expect_test "string with doubly escaped quotes" =
      test {| \\\"Leonard Bernstein!\\\" |} ;
      [%expect {| \"Leonard Bernstein!\" |}]
  end )

module Ascii_string = struct
  open Base_quickcheck

  type t = string [@@deriving sexp, quickcheck]

  let quickcheck_generator : t Generator.t =
    Generator.(
      string_of
        (char_uniform_inclusive (Char.of_int_exn 0) (Char.of_int_exn 127)))
end

let%test_unit "escape_string/read_string idempotent for ASCII" =
  Base_quickcheck.(
    Test.run_exn
      (module Ascii_string)
      ~f:(fun s ->
        [%test_result: string] ~equal:String.equal ~here:[[%here]] ~expect:s
          (read_string_from_string (Act_utils.Lex_utils.escape_string s))))
