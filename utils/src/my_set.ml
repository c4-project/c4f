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

open Core_kernel
open Travesty_core_kernel_exts
include My_set_intf

module Make_extensions (M : Set.S) : Extensions with type t := M.t = struct
  let disjoint x y = M.(is_empty (inter x y))

  module Partial_order = struct
    type t =
      | Equal
      | Subset of {in_right_only: M.t}
      | Superset of {in_left_only: M.t}
      | No_order of {in_left_only: M.t; in_right_only: M.t}
    [@@deriving sexp]

    (** [drop_left x p] updates partial order [p] with the information that
        an element [x] exists in the left hand set that isn't in the right
        hand set. *)
    let drop_left (x : M.Elt.t) : t -> t = function
      | Equal ->
          Superset {in_left_only= M.singleton x}
      | Superset {in_left_only} ->
          Superset {in_left_only= M.add in_left_only x}
      | Subset {in_right_only} ->
          No_order {in_left_only= M.singleton x; in_right_only}
      | No_order {in_left_only; in_right_only} ->
          No_order {in_left_only= M.add in_left_only x; in_right_only}

    (** [drop_right x p] updates partial order [p] with the information that
        an element [x] exists in the right hand set that isn't in the left
        hand set. *)
    let drop_right (x : M.Elt.t) : t -> t = function
      | Equal ->
          Subset {in_right_only= M.singleton x}
      | Subset {in_right_only} ->
          Subset {in_right_only= M.add in_right_only x}
      | Superset {in_left_only} ->
          No_order {in_left_only; in_right_only= M.singleton x}
      | No_order {in_left_only; in_right_only} ->
          No_order {in_left_only; in_right_only= M.add in_right_only x}

    let drop_either (po : t) : (M.Elt.t, M.Elt.t) Either.t -> t = function
      | First x ->
          drop_left x po
      | Second x ->
          drop_right x po

    let to_ordering_opt : t -> Ordering.t option = function
      | Equal ->
          Some Equal
      | Superset _ ->
          Some Greater
      | Subset _ ->
          Some Less
      | No_order _ ->
          None

    let is_equal : t -> bool = function
      | Equal ->
          true
      | Superset _ | Subset _ | No_order _ ->
          false

    let is_unordered : t -> bool = function
      | No_order _ ->
          true
      | Superset _ | Subset _ | Equal ->
          false

    let is_proper_superset : t -> bool = function
      | Superset _ ->
          true
      | No_order _ | Subset _ | Equal ->
          false

    let is_superset : t -> bool = Fn.(is_proper_superset ||| is_equal)

    let is_proper_subset : t -> bool = function
      | Subset _ ->
          true
      | No_order _ | Superset _ | Equal ->
          false

    let is_subset : t -> bool = Fn.(is_proper_subset ||| is_equal)

    let left_has_uniques : t -> bool = function
      | Superset _ | No_order _ ->
          true
      | Subset _ | Equal ->
          false

    let right_has_uniques : t -> bool = function
      | Subset _ | No_order _ ->
          true
      | Superset _ | Equal ->
          false

    let in_left_only : t -> M.t = function
      | Superset {in_left_only} | No_order {in_left_only; _} ->
          in_left_only
      | Subset _ | Equal ->
          M.empty

    let in_right_only : t -> M.t = function
      | Subset {in_right_only} | No_order {in_right_only; _} ->
          in_right_only
      | Superset _ | Equal ->
          M.empty
  end

  let partial_compare (x : M.t) (y : M.t) =
    Sequence.fold (M.symmetric_diff x y) ~init:Partial_order.Equal
      ~f:Partial_order.drop_either
end

module Extend (M : Set.S) : S with module Elt = M.Elt = struct
  include M
  include Make_extensions (M)
end

let%test_module "integer set" =
  ( module struct
    module M = Extend (Int.Set)

    let%expect_test "disjoint: positive witness" =
      printf "%b"
        (M.disjoint
           (Int.Set.of_list [2; 4; 6; 8])
           (Int.Set.of_list [3; 5; 7; 9])) ;
      [%expect {| true |}]

    let%expect_test "disjoint: negative witness" =
      let module M = Extend (Int.Set) in
      printf "%b"
        (M.disjoint
           (Int.Set.of_list [2; 4; 6; 8])
           (Int.Set.of_list [1; 2; 3; 4])) ;
      [%expect {| false |}]

    let%expect_test "disjoint: double empty is disjoint" =
      let module M = Extend (Int.Set) in
      printf "%b" (M.disjoint Int.Set.empty Int.Set.empty) ;
      [%expect {| true |}]

    let%expect_test "partial_compare: empty sets" =
      let module M = Extend (Int.Set) in
      Stdio.print_s
        [%sexp
          (M.partial_compare Int.Set.empty Int.Set.empty : M.Partial_order.t)] ;
      [%expect {| Equal |}]

    let%expect_test "partial_compare: subset" =
      let module M = Extend (Int.Set) in
      Stdio.print_s
        [%sexp
          ( Fn.on Int.Set.of_list ~f:M.partial_compare [1; 2; 3]
              [1; 2; 3; 4; 5; 6]
            : M.Partial_order.t )] ;
      [%expect {| (Subset (in_right_only (4 5 6))) |}]

    let%expect_test "partial_compare: superset" =
      Stdio.print_s
        [%sexp
          ( Fn.on Int.Set.of_list ~f:M.partial_compare [1; 2; 3; 4; 5; 6]
              [4; 5; 6]
            : M.Partial_order.t )] ;
      [%expect {| (Superset (in_left_only (1 2 3))) |}]

    let%expect_test "partial_compare: equal" =
      Stdio.print_s
        [%sexp
          ( Fn.on Int.Set.of_list ~f:M.partial_compare [1; 2; 3; 4; 5; 6]
              [6; 5; 4; 3; 2; 1]
            : M.Partial_order.t )] ;
      [%expect {| Equal |}]

    let%expect_test "partial_compare: no order" =
      Stdio.print_s
        [%sexp
          ( Fn.on Int.Set.of_list ~f:M.partial_compare [1; 2; 3; 4]
              [3; 4; 5; 6]
            : M.Partial_order.t )] ;
      [%expect {| (No_order (in_left_only (1 2)) (in_right_only (5 6))) |}]
  end )
