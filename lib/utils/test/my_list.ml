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
open Base_quickcheck
open Act_utils.My_list

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

module Int_list = struct
  type t = int list [@@deriving sexp, quickcheck]
end

let%test_unit "random_index is always in bounds" =
  let rng = Random.State.make_self_init ~allow_in_tests:true () in
  let random = Splittable_random.State.create rng in
  Test.run_exn (module Int_list)
    ~f:
      ([%test_pred: int list] ~here:[[%here]] (fun xs ->
           match random_index ~random xs with
           | None ->
               List.is_empty xs
           | Some i ->
               Option.is_some (List.nth xs i)))

let%expect_test "random item: empty list" =
  let deterministic_srng = Splittable_random.State.of_int 0 in
  print_s
    [%sexp (random_item ~random:deterministic_srng [] : int option)] ;
  [%expect {| () |}]

let%test_unit "random_item is always a valid item" =
  let rng = Random.State.make_self_init ~allow_in_tests:true () in
  let random = Splittable_random.State.create rng in
  Test.run_exn (module Int_list)
    ~f:
      ([%test_pred: int list] ~here:[[%here]] (fun xs ->
           match random_item ~random xs with
           | None ->
               List.is_empty xs
           | Some x ->
               List.mem ~equal:Int.equal xs x))


