(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

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
  print_s [%sexp (Sim_herd.Reader.read_output_from_string s : Sim.Output.t)]

let%expect_test "valid output without postcondition parses correctly" =
  print_output_from_string test_output_valid_no_pc ;
  [%expect
    {|
    (Success
     ((states
       (((A 100) (B 100) (C 1000) (D 100)) ((A 100) (B 0) (C 250) (D 50))
        ((A 0) (B 100) (C 50) (D 10)) ((A 0) (B 0) (C 0) (D 0))))
      (is_undefined false))) |}]
