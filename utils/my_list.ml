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
open Base_quickcheck

let replace (xs : 'a list) (at : int) ~(f : 'a -> 'a option Or_error.t) :
    'a list Or_error.t =
  let open Or_error.Let_syntax in
  let z_init = Zipper.Plain.of_list xs in
  let%bind z_move =
    Zipper.Plain.On_error.step_m z_init ~steps:at ~on_empty:(fun _ ->
        Or_error.error_s
          [%message
            "Replace failed: index out of range" ~here:[%here]
              ~insert_at:(at : int)
              ~list_length:(List.length xs : int)] )
  in
  let%map z_repl =
    Zipper.Plain.On_error.map_m_head z_move ~f ~on_empty:(fun _ ->
        Or_error.error_s
          [%message
            "Replace failed: index out of range" ~here:[%here]
              ~insert_at:(at : int)
              ~list_length:(List.length xs : int)] )
  in
  Zipper.Plain.to_list z_repl

let%expect_test "replace: successfully map" =
  let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
  let f x = Or_error.return (Some (String.uppercase x)) in
  let lst' = replace lst 2 ~f in
  Stdio.print_s [%sexp (lst' : string list Or_error.t)] ;
  [%expect {| (Ok (kappa keepo FRANKERZ pogchamp)) |}]

let%expect_test "replace: successfully delete" =
  let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
  let f _ = Or_error.return None in
  let lst' = replace lst 1 ~f in
  Stdio.print_s [%sexp (lst' : string list Or_error.t)] ;
  [%expect {| (Ok (kappa frankerz pogchamp)) |}]

let%expect_test "replace: failing function" =
  let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
  let f _ = Or_error.error_string "function failure" in
  let lst' = replace lst 3 ~f in
  Stdio.print_s [%sexp (lst' : string list Or_error.t)] ;
  [%expect {| (Error "function failure") |}]

let%expect_test "replace: out of bounds" =
  let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
  let f x = Or_error.return (Some (String.uppercase x)) in
  let lst' = replace lst 4 ~f in
  Stdio.print_s [%sexp (lst' : string list Or_error.t)] ;
  [%expect
    {|
    (Error
     ("Replace failed: index out of range" (here utils/my_list.ml:43:55)
      (insert_at 4) (list_length 4))) |}]

let insert (xs : 'a list) (at : int) (value : 'a) : 'a list Or_error.t =
  let open Or_error.Let_syntax in
  let z_init = Zipper.Plain.of_list xs in
  let%map z_move =
    Zipper.Plain.On_error.step_m z_init ~steps:at ~on_empty:(fun _ ->
        Or_error.error_s
          [%message
            "Insert failed: index out of range" ~here:[%here]
              ~insert_at:(at : int)
              ~list_length:(List.length xs : int)] )
  in
  let z_ins = Zipper.Plain.push z_move ~value in
  Zipper.Plain.to_list z_ins

let%test_module "insert" =
  ( module struct
    module Qc = struct
      type t = int * int list [@@deriving quickcheck, sexp]
    end

    let%test_unit "insert at 0 = cons" =
      Base_quickcheck.Test.run_exn
        (module Qc)
        ~f:(fun (x, xs) ->
          [%test_eq: int list] ~here:[[%here]]
            (Or_error.ok_exn (insert xs 0 x))
            (x :: xs) )
  end )

let find_at_most_one (type a b) ?(item_name : string = "item")
    (items : a list) ~(f : a -> b option) ~(on_empty : b Or_error.t) :
    b Or_error.t =
  Or_error.(
    match List.filter_map items ~f with
    | [] ->
        on_empty
    | [x] ->
        return x
    | _ ->
        errorf "Duplicate %s" item_name)

let find_one_opt (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b option Or_error.t =
  let f (x : a) : b option option =
    match f x with Some x -> Some (Some x) | None -> None
  in
  find_at_most_one items ~item_name ~f ~on_empty:(Or_error.return None)

let%test_module "find_one_opt" =
  ( module struct
    let f (x : int) : string option =
      Option.some_if (Int.is_pow2 x) (Int.to_string x)

    let p (s : string option Or_error.t) : unit =
      Stdio.print_s [%sexp (s : string option Or_error.t)]

    let%expect_test "find_one_opt: none" =
      p (find_one_opt ~f [3; 5; 11; 94]) ;
      [%expect {| (Ok ()) |}]

    let%expect_test "find_one_opt: one" =
      p (find_one_opt ~f [3; 4; 5; 11; 94]) ;
      [%expect {| (Ok (4)) |}]

    let%expect_test "find_one_opt: multiple" =
      p (find_one_opt ~f [3; 4; 5; 11; 64; 94]) ;
      [%expect {| (Error "Duplicate item") |}]
  end )

let find_one (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b Or_error.t =
  find_at_most_one items ~item_name ~f
    ~on_empty:(Or_error.errorf "Expected at least one %s" item_name)
