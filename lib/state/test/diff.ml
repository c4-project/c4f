(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Src = Act_state

let print (cmp : Src.Diff.t) : unit = Src.Diff.pp Fmt.stdout cmp

module Test_cases = struct
  let to_entry (k : int) : Src.Entry.t =
    Or_error.ok_exn
      Or_error.Let_syntax.(
        let%bind name = Act_common.Litmus_id.global_of_string "x" in
        let ks = Int.to_string k in
        Src.Entry.of_alist [(name, ks)])

  let to_entry_set : int list -> Set.M(Src.Entry).t =
    Fn.compose (Set.of_list (module Src.Entry)) (List.map ~f:to_entry)

  let lift (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit)
      (l : int list) (r : int list) : unit =
    f (to_entry_set l) (to_entry_set r)

  let with_empty_sets (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit)
      : unit =
    lift f [] []

  let with_equal_sets (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit)
      : unit =
    lift f [1; 2; 3; 4; 5; 6] [6; 5; 4; 3; 2; 1]

  let with_subset_sets
      (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit) : unit =
    lift f [1; 2; 3] [1; 2; 3; 4; 5; 6]

  let with_superset_sets
      (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit) : unit =
    lift f [6; 5; 4; 3; 2; 1] [3; 2; 1]

  let with_unordered_sets
      (f : Set.M(Src.Entry).t -> Set.M(Src.Entry).t -> unit) : unit =
    lift f [1; 2; 3; 4] [3; 4; 5; 6]
end

let%test_module "make" =
  ( module struct
    open Test_cases

    let test (x : Set.M(Src.Entry).t) (y : Set.M(Src.Entry).t) : unit =
      print (Src.Diff.make x y)

    let%expect_test "empty sets" = with_empty_sets test ; [%expect {| |}]

    let%expect_test "subset" =
      with_subset_sets test ;
      [%expect
        {|
        In subject only:
          {{x = 4}, {x = 5}, {x = 6}} |}]

    let%expect_test "superset" =
      with_superset_sets test ;
      [%expect
        {|
        In oracle only:
          {{x = 4}, {x = 5}, {x = 6}} |}]

    let%expect_test "equal" = with_equal_sets test ; [%expect {| |}]

    let%expect_test "no order" =
      with_unordered_sets test ;
      [%expect
        {|
        In oracle only:
          {{x = 1}, {x = 2}}
        In subject only:
          {{x = 5}, {x = 6}} |}]
  end )

let%test_module "to ordering" =
  ( module struct
    open Test_cases

    let test (x : Set.M(Src.Entry).t) (y : Set.M(Src.Entry).t) : unit =
      let spo = Src.Diff.make x y in
      print_s [%sexp (Src.Diff.to_ordering_opt spo : Ordering.t option)]

    let%expect_test "empty sets" =
      with_empty_sets test ; [%expect {| (Equal) |}]

    let%expect_test "subset" =
      with_subset_sets test ; [%expect {| (Less) |}]

    let%expect_test "superset" =
      with_superset_sets test ; [%expect {| (Greater) |}]

    let%expect_test "equal" = with_equal_sets test ; [%expect {| (Equal) |}]

    let%expect_test "no order" =
      with_unordered_sets test ; [%expect {| () |}]
  end )
