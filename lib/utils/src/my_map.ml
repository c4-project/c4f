(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let map_with_keys (type ak av aw bk bv bw) (cmp : (bk, bw) Map.comparator)
    (map : (ak, av, aw) Map.t) ~(f : key:ak -> data:av -> bk * bv) :
    (bk, bv, bw) Map.t Or_error.t =
  map |> Map.to_alist
  |> List.map ~f:(fun (key, data) -> f ~key ~data)
  |> Map.of_alist_or_error cmp

let not_found_error (type k) ?(sexp_of_key : (k -> Sexp.t) option)
    ~(map_name : string) (key : k) : Error.t =
  let string = Printf.sprintf "Given key not found in %s" map_name in
  match sexp_of_key with
  | None ->
      Error.of_string string
  | Some f ->
      Error.create_s [%message string ~key:(f key : Sexp.t)]

let find_or_error (type k v w) ?(sexp_of_key : (k -> Sexp.t) option)
    ?(map_name : string = "map") (map : (k, v, w) Map.t) (key : k) :
    v Or_error.t =
  let error = lazy (not_found_error ?sexp_of_key ~map_name key) in
  key |> Map.find map |> Result.of_option ~error:(Error.of_lazy_t error)

let merge_with_overlap (type k v w) (maps : (k, v, w) Map.t list)
    ~(compare : v -> v -> int) : (k, v, w) Map.t Or_error.t =
  match maps with
  | [] ->
      Or_error.error_string "empty"
  | m :: _ ->
      let cmp1 = (Map.comparator m).compare in
      maps
      |> List.concat_map ~f:Map.to_alist
      |> List.dedup_and_sort
           ~compare:(Core_kernel.Tuple2.compare ~cmp1 ~cmp2:compare)
      |> Map.of_alist_or_error (Map.comparator_s m)
