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

type 'a partial_order =
  [ `Equal
  | `Subset of 'a
  | `Superset of 'a
  | `NoOrder
  ] [@@deriving sexp]
;;

module type Extensions = sig
  type t

  val disjoint : t -> t -> bool
  val partial_compare : t -> t -> t partial_order
end

module Extend (S : Set.S) : Extensions with type t := S.t = struct
  let disjoint x y = S.(is_empty (inter x y));;

  (** [drop_left x p] updates partial order [p] with the information that
      an element [x] exists in the left hand set that isn't in the right
      hand set. *)
  let drop_left x = function
    | `Equal -> `Superset (S.singleton x)
    | `Superset xs -> `Superset (S.add xs x)
    | `NoOrder | `Subset _ -> `NoOrder
  ;;

  (** [drop_right x p] updates partial order [p] with the information
     that an element [x] exists in the right hand set that isn't in
     the left hand set. *)
  let drop_right x = function
    | `Equal -> `Subset (S.singleton x)
    | `Subset xs -> `Subset (S.add xs x)
    | `Superset _ | `NoOrder -> `NoOrder
  ;;

  let partial_compare x y =
    Sequence.fold (S.symmetric_diff x y)
      ~init:`Equal
      ~f:(fun po ->
          function
          | First  x -> drop_left  x po
          | Second x -> drop_right x po
        )
  ;;
end

let%expect_test "disjoint: positive witness" =
  let module M = Extend (Int.Set) in
  printf "%b"
    (M.disjoint
       (Int.Set.of_list [2; 4; 6; 8])
       (Int.Set.of_list [3; 5; 7; 9]));
  [%expect {| true |}]

let%expect_test "disjoint: negative witness" =
  let module M = Extend (Int.Set) in
  printf "%b"
    (M.disjoint
       (Int.Set.of_list [2; 4; 6; 8])
       (Int.Set.of_list [1; 2; 3; 4]));
  [%expect {| false |}]

let%expect_test "disjoint: double empty is disjoint" =
  let module M = Extend (Int.Set) in
  printf "%b" (M.disjoint (Int.Set.empty) (Int.Set.empty));
  [%expect {| true |}]

let%expect_test "partial_compare: empty sets" =
  let module M = Extend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( M.partial_compare (Int.Set.empty) (Int.Set.empty)
             : Int.Set.t partial_order
           )];
  [%expect {| Equal |}]

let%expect_test "partial_compare: subset" =
  let module M = Extend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3 ] [ 1; 2; 3; 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| (Subset (4 5 6)) |}]

let%expect_test "partial_compare: superset" =
  let module M = Extend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4; 5; 6 ] [ 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| (Superset (1 2 3)) |}]

let%expect_test "partial_compare: equal" =
  let module M = Extend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4; 5; 6 ] [ 6; 5; 4; 3; 2; 1 ]
             : Int.Set.t partial_order
           )];
  [%expect {| Equal |}]

let%expect_test "partial_compare: no order" =
  let module M = Extend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4 ] [ 3; 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| NoOrder |}]

