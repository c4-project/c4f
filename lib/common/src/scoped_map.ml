(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

type 'a t = 'a Map.M(Litmus_id).t

let equal (type a) : (a -> a -> bool) -> a t -> a t -> bool = Map.equal

let empty (type a) : a t = Map.empty (module Litmus_id)

let of_litmus_id_map (type a) : a Map.M(Litmus_id).t -> a t = Fn.id

let of_litmus_id_alist (type a) (xs : (Litmus_id.t, a) List.Assoc.t) :
    a t Or_error.t =
  (* yay value restriction *)
  Map.of_alist_or_error (module Litmus_id) xs

let set (type a) (m : a t) ~(id : Litmus_id.t) ~(record : a) : a t =
  Map.set m ~key:id ~data:record

let map_record (type a) (m : a t) ~(f : a -> a) ~(id : Litmus_id.t) : a t =
  Map.change m id ~f:(Option.map ~f)

let filter (type a) : a t -> f:(a -> bool) -> a t = Map.filter

let length (type a) : a t -> int = Map.length

let build_set (type a e w)
    (module Carrier : Comparable.S
      with type t = e
       and type comparator_witness = w) (m : a t)
    ~(f : Litmus_id.t -> a -> e option) : Set.M(Carrier).t =
  Map.fold m
    ~init:(Set.empty (module Carrier))
    ~f:(fun ~key ~data set ->
      Option.value_map (f key data) ~default:set ~f:(Set.add set))

let c_id_mem (m : _ t) ~(id : C_id.t) : bool =
  Map.existsi m ~f:(fun ~key ~data ->
      ignore data ;
      [%equal: C_id.t] id (Litmus_id.variable_name key))

let find_by_litmus_id (type a) (m : a t) ~(id : Litmus_id.t) : a Or_error.t =
  id |> Map.find m
  |> Result.of_option
       ~error:
         Error.(
           of_lazy_t
             ( lazy
               (create_s
                  [%message
                    "Litmus identifier doesn't match any in the map"
                      ~id:(id : Litmus_id.t)]) ))

let resolve (vars : _ t) ~(id : C_id.t) ~(scope : Scope.t) : Litmus_id.t =
  let global_id = Litmus_id.global id in
  match scope with
  | Global ->
      global_id
  | Local tid ->
      let local_id = Litmus_id.local tid id in
      if Map.mem vars local_id then local_id else global_id

let to_litmus_id_map (type a) : a t -> a Map.M(Litmus_id).t = Fn.id

let filter_in_scope (type a) ~(scope : Scope.t) ~(key : Litmus_id.t)
    ~(data : a) : (Scope.t * a) option =
  Option.some_if
    (Litmus_id.is_in_scope ~scope key)
    (Litmus_id.scope key, data)

let to_c_id_map (type a) (map : a t) ~(scope : Scope.t) : a Map.M(C_id).t =
  map
  |> Map.filter_mapi ~f:(filter_in_scope ~scope)
  |> Map.to_alist
  |> Tx.Alist.map_left ~f:Litmus_id.variable_name
  |> Map.of_alist_reduce (module C_id) ~f:Scope.reduce
  |> Map.map ~f:snd

module Make_json (Record : Plumbing.Jsonable_types.S) :
  Plumbing.Jsonable_types.S with type t = Record.t t =
  Plumbing.Jsonable.Make_map (Litmus_id) (Record)

let pp (type a) (pp_val : a Fmt.t) : a t Fmt.t =
  Fmt.(
    (* Trying to imitate fmt's record/field setup. *)
    using Map.to_alist
      (list ~sep:cut
         (box ~indent:1
            (pair ~sep:(any ":@ ") (styled `Yellow Litmus_id.pp) pp_val))))
