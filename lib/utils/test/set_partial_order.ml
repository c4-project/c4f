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
open Act_utils.Set_partial_order

let set_to_string (s : Set.M(Int).t) : string =
  s
  |> Set.to_sequence
  |> Sequence.map ~f:Int.to_string
  |> Sequence.intersperse ~sep:", "
  |> Sequence.to_list
  |> String.concat

let print (cmp : (int, Int.comparator_witness) t): unit =
  printf "{%s} | {%s}" (set_to_string (in_left_only cmp))
    (set_to_string (in_right_only cmp))

module Test_cases = struct
  let with_empty_sets (f : int list -> int list -> unit) : unit =
    f [] []

  let with_equal_sets (f : int list -> int list -> unit) : unit =
    f [1; 2; 3; 4; 5; 6] [6; 5; 4; 3; 2; 1]

  let with_subset_sets (f : int list -> int list -> unit) : unit =
    f [1; 2; 3] [1; 2; 3; 4; 5; 6]

  let with_superset_sets (f : int list -> int list -> unit) : unit =
    f [6; 5; 4; 3; 2; 1] [3; 2; 1]

  let with_unordered_sets (f : int list -> int list -> unit) : unit =
    f [1; 2; 3; 4] [3; 4; 5; 6]
end

let%test_module "make" = (module struct
  open Test_cases

  let test (x : int list) (y : int list) : unit =
    print (make (Set.of_list (module Int) x)
             (Set.of_list (module Int) y))

  let%expect_test "empty sets" =
    with_empty_sets test;
    [%expect {| {} | {} |}]

  let%expect_test "subset" =
    with_subset_sets test;
  [%expect {| {} | {4, 5, 6} |}]

  let%expect_test "superset" =
    with_superset_sets test;
    [%expect {| {4, 5, 6} | {} |}]

  let%expect_test "equal" =
    with_equal_sets test;
  [%expect {| {} | {} |}]

  let%expect_test "no order" =
    with_unordered_sets test;
  [%expect {| {1, 2} | {5, 6} |}]
end )
