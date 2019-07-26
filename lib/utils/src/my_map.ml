(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let map_with_keys (type ak av aw bk bv bw) (cmp : (bk, bw) Map.comparator)
    (map : (ak, av, aw) Map.t) ~(f : key:ak -> data:av -> bk * bv) :
    (bk, bv, bw) Map.t Or_error.t =
  map |> Map.to_alist
  |> List.map ~f:(fun (key, data) -> f ~key ~data)
  |> Map.of_alist_or_error cmp
