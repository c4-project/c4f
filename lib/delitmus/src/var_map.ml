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
    {c_type: Act_c_mini.Type.t; c_id: Act_common.C_id.t; mapped_to_global: bool}
  [@@deriving fields, yojson, equal]

  let make = Fields.create
end

type t = Record.t Ac.Scoped_map.t [@@deriving equal]

let lookup_and_require_global (map : t) ~(id : Ac.Litmus_id.t) :
    Ac.C_id.t Or_error.t =
  Or_error.Let_syntax.(match%bind Ac.Scoped_map.find_by_litmus_id map ~id with
  | {mapped_to_global= false; _} ->
      Or_error.error_s
        [%message
          "Litmus identifier was mapped to something other than a global \
           variable"
            ~id:(id : Ac.Litmus_id.t)]
  | {c_id= x; _} ->
      Or_error.return x)

let filter_to_alist (pred : Record.t -> bool) : t -> (Ac.Litmus_id.t, Record.t) List.Assoc.t =
  Tx.Fn.Compose_syntax.(Ac.Scoped_map.filter ~f:pred >> Ac.Scoped_map.to_litmus_id_map >> Map.to_alist)

let globally_unmapped_vars : t -> (Ac.Litmus_id.t, Record.t) List.Assoc.t =
  filter_to_alist (Fn.non Record.mapped_to_global)

let globally_mapped_vars : t -> (Ac.Litmus_id.t, Record.t) List.Assoc.t =
  filter_to_alist Record.mapped_to_global

let global_c_variables : t -> Set.M(Ac.C_id).t =
  Ac.Scoped_map.build_set
    (module Ac.C_id)
    ~f:(fun _ r -> Record.(Option.some_if (mapped_to_global r) (c_id r)))

module Json = Ac.Scoped_map.Make_json (Record)
include (Json : module type of Json with type t := t)