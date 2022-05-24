(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Sx = String_extended
module Tx = Travesty_base_exts

module T = struct
  (** [t] is the type of compiler IDs. *)
  type t = String.Caseless.t list [@@deriving compare, hash, sexp]

  let allowed_id_splits : char list = ['.'; ' '; '/'; '\\']

  let of_string : string -> t = String.split_on_chars ~on:allowed_id_splits

  let to_string : t -> string = String.concat ~sep:"."

  let module_name : string = "act.C4f_common.Id"
end

include T
include Identifiable.Make (T)

let of_string_list : string list -> t = Fn.id

let empty : t = []

let ( @: ) : string -> t -> t = List.cons

let ( @. ) : t -> t -> t = ( @ )

let hd_reduce (id : t) ~(on_empty : unit -> 'a) ~(f : string -> t -> 'a) : 'a
    =
  match id with [] -> on_empty () | x :: xs -> f x xs

let edit_distance : t -> t -> int = Tx.Fn.on to_string ~f:Sx.edit_distance

let suggestions (assoc : (t, 'a) List.Assoc.t) (id : t) : t list =
  let all =
    assoc
    |> List.map ~f:(fun (id', _) -> (id', edit_distance id id'))
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:snd)
    |> List.map ~f:fst
  in
  List.take all 5

let error_with_suggestions (assoc : (t, _) List.Assoc.t) (id : t)
    ~(id_type : string) () : 'a Or_error.t =
  let sgs = suggestions assoc id in
  Or_error.error_s
    [%message
      "unknown ID" ~of_type:id_type ~(id : t) ~suggestions:(sgs : t list)]

let try_find_assoc_with_suggestions (assoc : (t, 'a) List.Assoc.t) (id : t)
    ~(id_type : string) : 'a Or_error.t =
  List.Assoc.find assoc ~equal id
  |> Option.map ~f:Or_error.return
  |> Tx.Option.value_f ~default_f:(error_with_suggestions assoc id ~id_type)

let to_string_list : t -> string list = Fn.id

let is_prefix id ~prefix =
  List.is_prefix (to_string_list id) ~prefix:(to_string_list prefix)
    ~equal:String.Caseless.equal

let try_match_prefix (prefix : t) (value : 'v) ~(full_id : t) :
    (t * 'v) option =
  Option.some_if (is_prefix full_id ~prefix) (prefix, value)

let try_find_assoc_with_suggestions_prefix (assoc : (t, 'a) List.Assoc.t)
    (full_id : t) ~(id_type : string) : (t * 'a) Or_error.t =
  List.find_map assoc ~f:(fun (k, v) -> try_match_prefix ~full_id k v)
  |> Option.map ~f:Or_error.return
  |> Tx.Option.value_f
       ~default_f:(error_with_suggestions assoc full_id ~id_type)

let drop_prefix (id : t) ~(prefix : t) : t Or_error.t =
  if is_prefix id ~prefix then
    Or_error.return (List.drop id (List.length prefix))
  else
    Or_error.error_s
      [%message
        "The given prefix ID is not a prefix of the other ID."
          ~(id : t)
          ~(prefix : t)]

let has_tag id element = List.mem id element ~equal:String.Caseless.equal

let pp_pair (ppe : 'e Fmt.t) : (t * 'e) Fmt.t =
  Fmt.(vbox ~indent:2 (pair (pp ++ any ":") ppe))

let pp_alist (ppe : 'e Fmt.t) : (t, 'e) List.Assoc.t Fmt.t =
  Fmt.(vbox (list ~sep:cut (pp_pair ppe)))

let pp_map (ppe : 'e Fmt.t) : (t, 'e, comparator_witness) Base.Map.t Fmt.t =
  Fmt.using Map.to_alist (pp_alist ppe)
