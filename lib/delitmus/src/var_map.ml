(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

type t = Ac.C_id.t option Map.M(Ac.Litmus_id).t

let equal : t -> t -> bool = Map.equal [%equal: Ac.C_id.t option]

let empty : t = Map.empty (module Ac.Litmus_id)

let of_map : Ac.C_id.t option Map.M(Ac.Litmus_id).t -> t = Fn.id

let of_set_with_qualifier (vars : Set.M(Ac.Litmus_id).t)
    ~(qualifier : Ac.Litmus_id.t -> Ac.C_id.t option) : t =
  let result =
    vars
    |> Set.to_sequence ~order:`Increasing
    |> Sequence.map ~f:(fun v -> (v, qualifier v))
    |> Map.of_increasing_sequence (module Ac.Litmus_id)
  in
  (* Assuming that the error introduced by Map.of_increasing_sequence is
     always either because the sequence is non-increasing (which contradicts
     '~order:`Increasing', or contains duplicate elements (which contradicts
     the sequence being built from a set). *)
  Or_error.ok_exn result

let lookup_and_require_global (map : t) ~(id : Ac.Litmus_id.t) :
    Ac.C_id.t Or_error.t =
  match Map.find map id with
  | None ->
      Or_error.error_s
        [%message
          "Litmus identifier doesn't match any in the auxiliary var map"
            ~id:(id : Ac.Litmus_id.t)]
  | Some None ->
      Or_error.error_s
        [%message
          "Litmus identifier was mapped to something other than a global \
           variable"
            ~id:(id : Ac.Litmus_id.t)]
  | Some (Some x) ->
      Or_error.return x

let build_set (type e w)
    (module Carrier : Comparable.S
      with type t = e
       and type comparator_witness = w)
    ~(f : Ac.Litmus_id.t -> Ac.C_id.t option -> e option) :
    t -> Set.M(Carrier).t =
  Map.fold
    ~init:(Set.empty (module Carrier))
    ~f:(fun ~key ~data set ->
      Option.value_map (f key data) ~default:set ~f:(Set.add set) )

let unmapped_litmus_ids : t -> Set.M(Ac.Litmus_id).t =
  build_set (module Ac.Litmus_id) ~f:(fun k data -> if Option.is_none data then Some k else None)

let globally_mapped_litmus_ids : t -> Set.M(Ac.Litmus_id).t =
  build_set (module Ac.Litmus_id) ~f:(fun k data -> if Option.is_some data then Some k else None)

let global_c_variables : t -> Set.M(Ac.C_id).t =
  build_set (module Ac.C_id) ~f:(fun _ data -> data)

module Json : Plumbing.Loadable_types.Jsonable with type t := t = struct
  let to_yojson (map : t) : Yojson.Safe.t =
    let alist =
      map |> Map.to_alist
      |> Tx.Alist.bi_map ~left:Ac.Litmus_id.to_string
           ~right:[%to_yojson: Ac.C_id.t option]
    in
    `Assoc alist

  module U = Yojson.Safe.Util

  let of_yojson_exn (json : Yojson.Safe.t) : t =
    json |> U.to_assoc
    |> Tx.Alist.bi_map ~left:Ac.Litmus_id.of_string ~right:(fun x ->
           x |> [%of_yojson: Ac.C_id.t option] |> Result.ok_or_failwith )
    |> Map.of_alist_exn (module Ac.Litmus_id)
    |> of_map

  let of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
    Result.try_with (fun () -> of_yojson_exn json)
    |> Result.map_error ~f:Exn.to_string
end

include Json
