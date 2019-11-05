(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Travesty_base_exts

type t = {in_left_only: Entry.Set.t; in_right_only: Entry.Set.t}
[@@deriving fields, yojson]

let empty : t =
  { in_left_only= Set.empty (module Entry)
  ; in_right_only= Set.empty (module Entry) }

(** [drop_left x p] updates partial order [p] with the information that an
    element [x] exists in the left hand set that isn't in the right hand set. *)
let drop_left (x : Entry.t) (p : t) : t =
  {p with in_left_only= Set.add p.in_left_only x}

(** [drop_right x p] updates partial order [p] with the information that an
    element [x] exists in the right hand set that isn't in the left hand set. *)
let drop_right (x : Entry.t) (p : t) : t =
  {p with in_right_only= Set.add p.in_right_only x}

let drop_either (po : t) : (Entry.t, Entry.t) Either.t -> t = function
  | First x ->
      drop_left x po
  | Second x ->
      drop_right x po

let to_ordering_opt (p : t) : Ordering.t option =
  match (Set.is_empty p.in_left_only, Set.is_empty p.in_right_only) with
  | false, false ->
      None
  | false, true ->
      Some Greater
  | true, true ->
      Some Equal
  | true, false ->
      Some Less

let ordering_eq (ord : Ordering.t option) (p : t) : bool =
  [%equal: Ordering.t option] ord (to_ordering_opt p)

let left_has_uniques (p : t) : bool = not (Set.is_empty p.in_left_only)

let right_has_uniques (p : t) : bool = not (Set.is_empty p.in_right_only)

(* nb: these aren't eta-reduced because of the value restriction. *)

let is_equal (p : t) : bool = ordering_eq (Some Equal) p

let is_unordered (p : t) : bool = ordering_eq None p

let is_proper_superset (p : t) : bool = ordering_eq (Some Greater) p

let is_proper_subset (p : t) : bool = ordering_eq (Some Less) p

let is_superset (x : t) : bool = Fn.(is_proper_superset ||| is_equal) x

let is_subset (x : t) : bool = Fn.(is_proper_subset ||| is_equal) x

let make (x : Set.M(Entry).t) (y : ('v, 'cmp) Set.t) : t =
  let init = empty in
  Sequence.fold (Set.symmetric_diff x y) ~init ~f:drop_either

let pp_side_set (side : string) (f : Formatter.t) (set : Set.M(Entry).t) :
    unit =
  if not (Set.is_empty set) then
    Fmt.pf f "@[<v 2>In %s only:@ %a@]" side
      (Act_utils.My_format.pp_set Entry.pp)
      set

let pp : t Fmt.t =
  Fmt.(
    using in_left_only (pp_side_set "oracle")
    ++ using is_unordered (Act_utils.My_format.pp_if sp nop)
    ++ using in_right_only (pp_side_set "subject"))
