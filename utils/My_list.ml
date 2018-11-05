(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core

include Fold_map.List

let%expect_test "MyList: max_measure on empty list" =
  printf "%d" (max_measure ~default:1066 ~measure:Fn.id []);
  [%expect {| 1066 |}]
;;

let exclude ~f xs = List.filter ~f:(Fn.non f) xs

let%expect_test "MyList: exclude -ve numbers" =
  let excluded = exclude ~f:Int.is_negative
      [1; -1; 2; 10; -49; 0; 64]
  in
  Format.printf "@[%a@]@."
    (Format.pp_print_list ~pp_sep:My_format.pp_csep Int.pp) excluded;
  [%expect {| 1, 2, 10, 0, 64 |}]
;;

let%expect_test "MyList: right_pad empty list" =
  Format.printf "@[%a@]@."
    (My_format.pp_listlist ~pp:Int.pp) (right_pad ~padding:2 []);
  [%expect {||}]
;;

let%expect_test "MyList: right_pad example list" =
  Format.printf "@[%a@]@."
    (My_format.pp_listlist ~pp:Int.pp)
    (right_pad ~padding:6
       [ [0; 8; 0; 0]
       ; [9; 9; 9]
       ; [8; 8; 1; 9; 9]
       ; [9; 1; 1; 9]
       ; [7; 2; 5]
       ; [3]
       ]);
  [%expect {|
                [ 0, 8, 0, 0, 6 ]
                [ 9, 9, 9, 6, 6 ]
                [ 8, 8, 1, 9, 9 ]
                [ 9, 1, 1, 9, 6 ]
                [ 7, 2, 5, 6, 6 ]
                [ 3, 6, 6, 6, 6 ] |}]
;;

let%expect_test "mapM: list" =
  let module M = On_monad (List) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_listlist ~pp:Int.pp)
    (List.bind ~f:(M.mapM ~f:(fun k -> [k; 0]))
       ([[1; 2; 3]]));
  [%expect {|
              [ 1, 2, 3 ]
              [ 1, 2, 0 ]
              [ 1, 0, 3 ]
              [ 1, 0, 0 ]
              [ 0, 2, 3 ]
              [ 0, 2, 0 ]
              [ 0, 0, 3 ]
              [ 0, 0, 0 ] |}]
;;

let prefixes xs =
  List.mapi ~f:(fun i _ -> List.take xs (i+1)) xs
;;

let%expect_test "prefixes: empty list" =
  Format.printf "@[<h>%a@]@."
    (My_format.pp_listlist ~pp:Int.pp)
    (prefixes []);
  [%expect {||}]
;;

let%expect_test "prefixes: sample list" =
  Format.printf "@[<h>%a@]@."
    (My_format.pp_listlist ~pp:Int.pp)
    (prefixes [1; 2; 3]);
  [%expect {|
              [ 1 ]
              [ 1, 2 ]
              [ 1, 2, 3 ] |}]
;;
