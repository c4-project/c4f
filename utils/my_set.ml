(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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
open Travesty

module Partial_order = struct
  type 'a t =
    | Equal
    | Subset of { in_right_only : 'a }
    | Superset of { in_left_only : 'a }
    | No_order of { in_left_only : 'a; in_right_only : 'a }
  [@@deriving sexp]
end

module type Extensions = sig
  type t

  val disjoint : t -> t -> bool
  val partial_compare : t -> t -> t Partial_order.t
end

module Extend (S : Set.S) : Extensions with type t := S.t = struct
  let disjoint x y = S.(is_empty (inter x y))

  (** [drop_left x p] updates partial order [p] with the information that
      an element [x] exists in the left hand set that isn't in the right
      hand set. *)
  let drop_left (x : S.Elt.t) : S.t Partial_order.t -> S.t Partial_order.t = function
    | Equal -> Superset { in_left_only = S.singleton x }
    | Superset { in_left_only } -> Superset { in_left_only = S.add in_left_only x }
    | Subset { in_right_only } ->
      No_order { in_left_only = S.singleton x; in_right_only }
    | No_order { in_left_only; in_right_only } ->
      No_order { in_left_only = S.add in_left_only x; in_right_only }
  ;;

  (** [drop_right x p] updates partial order [p] with the information
      that an element [x] exists in the right hand set that isn't in
      the left hand set. *)
  let drop_right (x : S.Elt.t) : S.t Partial_order.t -> S.t Partial_order.t = function
    | Equal -> Subset { in_right_only = S.singleton x }
    | Subset { in_right_only } -> Subset { in_right_only = S.add in_right_only x }
    | Superset { in_left_only } ->
      No_order { in_left_only; in_right_only = S.singleton x }
    | No_order { in_left_only; in_right_only } ->
      No_order { in_left_only; in_right_only = S.add in_right_only x }
  ;;

  let partial_compare x y =
    Sequence.fold (S.symmetric_diff x y) ~init:Partial_order.Equal ~f:(fun po -> function
      | First x -> drop_left x po
      | Second x -> drop_right x po)
  ;;
end

let%test_module "integer set" =
  (module struct
     module M = Extend (Int.Set)

     let%expect_test "disjoint: positive witness" =
       printf
         "%b"
         (M.disjoint (Int.Set.of_list [ 2; 4; 6; 8 ]) (Int.Set.of_list [ 3; 5; 7; 9 ]));
       [%expect {| true |}]
     ;;

     let%expect_test "disjoint: negative witness" =
       let module M = Extend (Int.Set) in
       printf
         "%b"
         (M.disjoint (Int.Set.of_list [ 2; 4; 6; 8 ]) (Int.Set.of_list [ 1; 2; 3; 4 ]));
       [%expect {| false |}]
     ;;

     let%expect_test "disjoint: double empty is disjoint" =
       let module M = Extend (Int.Set) in
       printf "%b" (M.disjoint Int.Set.empty Int.Set.empty);
       [%expect {| true |}]
     ;;

     let%expect_test "partial_compare: empty sets" =
       let module M = Extend (Int.Set) in
       Stdio.print_s
         [%sexp
           (M.partial_compare Int.Set.empty Int.Set.empty : Int.Set.t Partial_order.t)];
       [%expect {| Equal |}]
     ;;

     let%expect_test "partial_compare: subset" =
       let module M = Extend (Int.Set) in
       Stdio.print_s
         [%sexp
           (T_fn.on Int.Set.of_list M.partial_compare [ 1; 2; 3 ] [ 1; 2; 3; 4; 5; 6 ]
             : Int.Set.t Partial_order.t)];
       [%expect {| (Subset (in_right_only (4 5 6))) |}]
     ;;

     let%expect_test "partial_compare: superset" =
       let module M = Extend (Int.Set) in
       Stdio.print_s
         [%sexp
           (T_fn.on Int.Set.of_list M.partial_compare [ 1; 2; 3; 4; 5; 6 ] [ 4; 5; 6 ]
             : Int.Set.t Partial_order.t)];
       [%expect {| (Superset (in_left_only (1 2 3))) |}]
     ;;

     let%expect_test "partial_compare: equal" =
       let module M = Extend (Int.Set) in
       Stdio.print_s
         [%sexp
           (T_fn.on
              Int.Set.of_list
              M.partial_compare
              [ 1; 2; 3; 4; 5; 6 ]
              [ 6; 5; 4; 3; 2; 1 ]
             : Int.Set.t Partial_order.t)];
       [%expect {| Equal |}]
     ;;

     let%expect_test "partial_compare: no order" =
       let module M = Extend (Int.Set) in
       Stdio.print_s
         [%sexp
           (T_fn.on Int.Set.of_list M.partial_compare [ 1; 2; 3; 4 ] [ 3; 4; 5; 6 ]
             : Int.Set.t Partial_order.t)];
       [%expect {| (No_order (in_left_only (1 2)) (in_right_only (5 6))) |}]
     ;;
  end)
;;
