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

module Record = struct
  type t =
    {c_type: Act_c.Mini.Type.t; c_id: Act_common.C_id.t; is_global: bool}
  [@@deriving fields, yojson, equal]

  let make = Fields.create

  let is_not_global : t -> bool = Fn.non is_global
end

type t = Record.t Map.M(Ac.Litmus_id).t

let equal : t -> t -> bool = Map.equal [%equal: Record.t]

let empty : t = Map.empty (module Ac.Litmus_id)

let of_map : Record.t Map.M(Ac.Litmus_id).t -> t = Fn.id

let lookup_and_require_global (map : t) ~(id : Ac.Litmus_id.t) :
    Ac.C_id.t Or_error.t =
  match Map.find map id with
  | None ->
      Or_error.error_s
        [%message
          "Litmus identifier doesn't match any in the auxiliary var map"
            ~id:(id : Ac.Litmus_id.t)]
  | Some {is_global= false; _} ->
      Or_error.error_s
        [%message
          "Litmus identifier was mapped to something other than a global \
           variable"
            ~id:(id : Ac.Litmus_id.t)]
  | Some {c_id= x; _} ->
      Or_error.return x

let build_set (type e w)
    (module Carrier : Comparable.S
      with type t = e
       and type comparator_witness = w)
    ~(f : Ac.Litmus_id.t -> Record.t -> e option) : t -> Set.M(Carrier).t =
  Map.fold
    ~init:(Set.empty (module Carrier))
    ~f:(fun ~key ~data set ->
      Option.value_map (f key data) ~default:set ~f:(Set.add set))

let globally_unmapped_vars : t -> (Ac.Litmus_id.t, Record.t) List.Assoc.t =
  Tx.Fn.Compose_syntax.(Map.filter ~f:Record.is_not_global >> Map.to_alist)

let globally_mapped_vars : t -> (Ac.Litmus_id.t, Record.t) List.Assoc.t =
  Tx.Fn.Compose_syntax.(Map.filter ~f:Record.is_global >> Map.to_alist)

let global_c_variables : t -> Set.M(Ac.C_id).t =
  build_set
    (module Ac.C_id)
    ~f:(fun _ r -> Record.(Option.some_if (is_global r) (c_id r)))

module Json : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Make_map (Ac.Litmus_id) (Record)

include Json
