(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_utils.Set_partial_order

let set_to_string (s : Set.M(Int).t) : string =
  s |> Set.to_sequence
  |> Sequence.map ~f:Int.to_string
  |> Sequence.intersperse ~sep:", "
  |> Sequence.to_list |> String.concat

let print (cmp : (int, Int.comparator_witness) t) : unit =
  printf "{%s} | {%s}"
    (set_to_string (in_left_only cmp))
    (set_to_string (in_right_only cmp))

module Test_cases = struct
  let lift (f : Set.M(Int).t -> Set.M(Int).t -> unit)
    (l : int list) (r : int list) : unit =
    f (Set.of_list (module Int) l) (Set.of_list (module Int) r)

  let with_empty_sets (f : Set.M(Int).t -> Set.M(Int).t -> unit) : unit = lift f [] []

  let with_equal_sets
(f : Set.M(Int).t -> Set.M(Int).t -> unit) : unit =
    lift f [1; 2; 3; 4; 5; 6] [6; 5; 4; 3; 2; 1]

  let with_subset_sets
(f : Set.M(Int).t -> Set.M(Int).t -> unit) : unit =
    lift f [1; 2; 3] [1; 2; 3; 4; 5; 6]

  let with_superset_sets
(f : Set.M(Int).t -> Set.M(Int).t -> unit) : unit =
    lift f [6; 5; 4; 3; 2; 1] [3; 2; 1]

  let with_unordered_sets
(f : Set.M(Int).t -> Set.M(Int).t -> unit) : unit =
    lift f [1; 2; 3; 4] [3; 4; 5; 6]
end

let%test_module "make" =
  ( module struct
    open Test_cases

    let test (x : Set.M(Int).t) (y : Set.M(Int).t) : unit =
      print (make x y)

    let%expect_test "empty sets" =
      with_empty_sets test ; [%expect {| {} | {} |}]

    let%expect_test "subset" =
      with_subset_sets test ; [%expect {| {} | {4, 5, 6} |}]

    let%expect_test "superset" =
      with_superset_sets test ; [%expect {| {4, 5, 6} | {} |}]

    let%expect_test "equal" = with_equal_sets test ; [%expect {| {} | {} |}]

    let%expect_test "no order" =
      with_unordered_sets test ; [%expect {| {1, 2} | {5, 6} |}]
  end )

let%test_module "to ordering" = (module struct
    open Test_cases

    let test (x : Set.M(Int).t) (y : Set.M(Int).t) : unit =
       let spo = (make x y) in
       print_s [%sexp (to_ordering_opt spo : Ordering.t option)]

    let%expect_test "empty sets" =
      with_empty_sets test ; [%expect {| (Equal) |}]

    let%expect_test "subset" =
      with_subset_sets test ; [%expect {| (Less) |}]

    let%expect_test "superset" =
      with_superset_sets test ; [%expect {| (Greater) |}]

    let%expect_test "equal" = with_equal_sets test ; [%expect {| (Equal) |}]

    let%expect_test "no order" =
      with_unordered_sets test ; [%expect {| () |}]  
end)
