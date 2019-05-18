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
open Travesty_base_exts

type ('v, 'cmp) t =
  { in_left_only : ('v, 'cmp) Set.t
  ; in_right_only : ('v, 'cmp) Set.t
  } [@@deriving fields]

let init (c : ('v, 'cmp) Set.comparator) : ('v, 'cmp) t =
  { in_left_only = Set.empty c
  ; in_right_only = Set.empty c
  }

(** [drop_left x p] updates partial order [p] with the information that
    an element [x] exists in the left hand set that isn't in the right
    hand set. *)
let drop_left (x : 'v) (p : ('v, 'cmp) t) : ('v, 'cmp) t =
  { p with in_left_only = Set.add p.in_left_only x }

(** [drop_right x p] updates partial order [p] with the information that
    an element [x] exists in the right hand set that isn't in the left
    hand set. *)
let drop_right (x : 'v) (p : ('v, 'cmp) t) : ('v, 'cmp) t =
  { p with in_right_only = Set.add p.in_right_only x }

let drop_either (po : ('v, 'cmp) t) : ('v, 'v) Either.t -> ('v, 'cmp) t = function
  | First x ->
    drop_left x po
  | Second x ->
    drop_right x po

let to_ordering_opt (p : (_, _) t) : Ordering.t option =
  match Set.is_empty p.in_left_only, Set.is_empty p.in_right_only with
  | (false, false) -> None
  | (false, true) -> Some Less
  | (true, true) -> Some Equal
  | (true, false) -> Some Greater

let ordering_eq (ord : Ordering.t option) (p : (_, _) t) : bool =
  [%equal: Ordering.t option] ord (to_ordering_opt p)

let left_has_uniques (p : (_, _) t) : bool =
  not (Set.is_empty p.in_left_only)

let right_has_uniques (p : (_, _) t) : bool =
  not (Set.is_empty p.in_right_only)

(* nb: these aren't eta-reduced because of the value restriction. *)

let is_equal (p : (_, _) t) : bool = ordering_eq (Some Equal) p

let is_unordered (p : (_, _) t) : bool = ordering_eq None p

let is_proper_superset (p : (_, _) t) : bool = ordering_eq (Some Greater) p

let is_proper_subset (p : (_, _) t) : bool = ordering_eq (Some Less) p


let is_superset (x : (_, _) t) : bool = Fn.(is_proper_superset ||| is_equal) x

let is_subset (x : (_, _) t) : bool = Fn.(is_proper_subset ||| is_equal) x

let make (x : ('v, 'cmp) Set.t) (y : ('v, 'cmp) Set.t) : ('v, 'cmp) t =
  let init = init (Set.comparator_s x) in
  Sequence.fold (Set.symmetric_diff x y) ~init ~f:drop_either

module M (C : sig type t type comparator_witness end) = struct
  type nonrec t = (C.t, C.comparator_witness) t
end
