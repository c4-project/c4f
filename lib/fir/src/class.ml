(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make_ext (B : Class_types.S) :
  Class_types.S_ext with type t := B.t and type 'meta elt := 'meta B.elt =
struct
  include B

  let class_matches_any (clazz : t) ~(templates : t list) : bool =
    List.exists templates ~f:(fun template -> class_matches clazz ~template)

  let class_unmatches_any (clazz : t) ~(templates : t list) : bool =
    List.exists templates ~f:(fun template ->
        not (class_matches clazz ~template))

  let matches_any (type e) (stm : e elt) ~(templates : t list) : bool =
    Option.exists (classify stm) ~f:(class_matches_any ~templates)

  let unmatches_any (type e) (stm : e elt) ~(templates : t list) : bool =
    Option.for_all (classify stm) ~f:(class_unmatches_any ~templates)

  let count_rec_matches (type e) (stm : e elt) ~(templates : t list) : int =
    List.count (classify_rec stm) ~f:(class_matches_any ~templates)

  let rec_matches_any (type e) (stm : e elt) ~(templates : t list) : bool =
    List.exists (classify_rec stm) ~f:(class_matches_any ~templates)

  let rec_unmatches_any (type e) (stm : e elt) ~(templates : t list) : bool =
    List.exists (classify_rec stm) ~f:(class_unmatches_any ~templates)

  let satisfies (type e) (elt : e elt) ~(req : Class_constraint.t)
      ~(templates : t list) : bool =
    match req with
    | Is ->
        matches_any elt ~templates
    | Is_not_any ->
        not (matches_any elt ~templates)
    | Is_not_one ->
        unmatches_any elt ~templates
    | Has ->
        rec_matches_any elt ~templates
    | Has_not_any ->
        not (rec_matches_any elt ~templates)
    | Has_not_one ->
        rec_unmatches_any elt ~templates
end

let lift_classify_rec (type m t) (f : m -> t option) (x : m) : t list =
  Option.to_list (f x)
