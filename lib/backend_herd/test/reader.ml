(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Stdio

(** An example 'valid' Herd7 output with no postcondition. *)
let test_output_valid_no_pc : string =
  {|
Test foo Required
States 4
A=0; B=0; C=0; D=0;
A=0; B=100; C=50; D=10;
A=100; B=0; C=250; D=50;
A=100; B=100; C=1000; D=100;
Ok
Witnesses
Positive: 15 Negative: 0
Condition forall (true)
Observation foo Always 15 0
Time foo 0.02
Hash=7bfcb06facbe75eff0dc2e656dd34eb4
|}

let print_output_from_string (s : string) : unit =
  print_s
    [%sexp
      (Act_backend_herd.Reader.read_output_from_string s : Act_backend.Output.t)]

let%expect_test "valid output without postcondition parses correctly" =
  print_output_from_string test_output_valid_no_pc ;
  [%expect
    {|
    (Success
     ((states
       (((A 100) (B 100) (C 1000) (D 100)) ((A 100) (B 0) (C 250) (D 50))
        ((A 0) (B 100) (C 50) (D 10)) ((A 0) (B 0) (C 0) (D 0))))
      (is_undefined false))) |}]
