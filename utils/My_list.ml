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

open Core_kernel

type 'a t = 'a list

include Traversable.Make_container1 (struct
    type 'a t = 'a list

    module On_monad (M : Monad.S) = struct
      let mapM xs ~f =
        let open M.Let_syntax in
        let%map xs_final =
          List.fold_left xs
            ~init:(return [])
            ~f:(fun state x ->
                let%bind xs' = state in
                let%map  x'  = f x in
                x' :: xs')
        in
        List.rev xs_final
      ;;
    end
  end)
;;

let%expect_test "generated list map behaves properly" =
  Format.printf "@[%a@]@."
    (Format.pp_print_list Int.pp ~pp_sep:Format.pp_print_space)
    (map ~f:(fun x -> x * x) [ 1; 3; 5; 7 ]);
  [%expect {| 1 9 25 49 |}]
;;

let%expect_test "generated list count behaves properly" =
  Format.printf "@[%d@]@." (count ~f:Int.is_positive [ -7; -5; -3; -1; 1; 3; 5; 7 ]);
  [%expect {| 4 |}]
;;

let%expect_test "mapiM: returning identity on list/option" =
  let module M = On_monad (Option) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:My_format.pp_csep String.pp))
    (M.mapiM ~f:(fun _ k -> Some k) ["a"; "b"; "c"; "d"; "e"]);
  [%expect {| a, b, c, d, e |}]

let%expect_test "mapiM: counting upwards on list/option" =
  let module M = On_monad (Option) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:My_format.pp_csep Int.pp))
    (M.mapiM ~f:(fun i _ -> Some i) [3; 7; 2; 4; 42]);
  [%expect {| 0, 1, 2, 3, 4 |}]

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

let one = function
  | [x] -> Ok x
  | xs ->
    Or_error.error_s
      [%message "Expected one element" ~got:(List.length xs : int)]
;;

let%expect_test "one: zero elements" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (one [] : int Or_error.t)];
  [%expect {| (Error ("Expected one element" (got 0))) |}]
;;

let%expect_test "one: one element" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (one [ 42 ] : int Or_error.t)];
  [%expect {| (Ok 42) |}]
;;

let%expect_test "one: two elements" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (one [ 27; 53 ] : int Or_error.t)];
  [%expect {| (Error ("Expected one element" (got 2))) |}]
;;

let two = function
  | [x; y] -> Ok (x, y)
  | xs ->
    Or_error.error_s
      [%message "Expected two elements" ~got:(List.length xs : int)]
;;

let%expect_test "one: one element" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (two [ 42 ] : (int * int) Or_error.t)];
  [%expect {| (Error ("Expected two elements" (got 1))) |}]
;;

let%expect_test "one: two elements" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (two [ 27; 53 ] : (int * int) Or_error.t)];
  [%expect {| (Ok (27 53)) |}]
;;

let%expect_test "one: three elements" =
  Sexp.output_hum Out_channel.stdout
    [%sexp (two [ "veni"; "vidi"; "vici" ] : (string * string) Or_error.t)];
  [%expect {| (Error ("Expected two elements" (got 3))) |}]
;;
