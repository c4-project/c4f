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

open Base
open Stdio
open Act_utils.Tabulator

let%expect_test "Sample use of tabulator with 'add_rows'" =
  let tab_result =
    Result.(
      make ~header:["Item"; "Quantity"; "Available"] ()
      >>= add_rows
            ~rows:
              [ ["Bicycle"; "5"; "false"]
              ; ["Car"; "10"; "true"]
              ; ["African Swallow"; "1"; "true"] ]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item             Quantity  Available
    Bicycle          5         false
    Car              10        true
    African Swallow  1         true
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with rules" =
  let tab_result =
    Result.(
      make ~header:["Item"; "Quantity"; "Available"] ()
      >>= add_rule ~char:'='
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_rule ~char:'-'
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item             Quantity  Available
    ====================================
    Bicycle          5         false
    Car              10        true
    ------------------------------------
    African Swallow  1         true
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with custom separators" =
  let tab_result =
    Result.(
      make ~sep:" | " ~terminator:";"
        ~header:["Item"; "Quantity"; "Available"]
        ()
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item            | Quantity | Available;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    African Swallow | 1        | true     ;
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with custom separators and rule" =
  let tab_result =
    Result.(
      make ~sep:" | " ~terminator:";"
        ~header:["Item"; "Quantity"; "Available"]
        ()
      >>= add_rule ~char:'='
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_rule ~char:'-'
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item            | Quantity | Available;
    ======================================;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    --------------------------------------;
    African Swallow | 1        | true     ;
    (Ok ()) |}]
