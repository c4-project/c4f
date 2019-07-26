(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = Local of int | Global [@@deriving compare, equal]

let is_global : t -> bool = function Local _ -> false | Global -> true

let of_litmus_id (id : Litmus_id.t) : t =
  match Litmus_id.tid id with Some t -> Local t | None -> Global

let id_in_scope (scope : t) ~(id : Litmus_id.t) : bool =
  match scope with
  | Global ->
      Litmus_id.is_global id
  | Local from ->
      Litmus_id.is_in_scope id ~from

let reduce (type a) (l : t * a) (r : t * a) : t * a =
  if Comparable.lift ~f:fst [%compare: t] l r <= 0 then l else r
