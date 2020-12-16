(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Mapping = struct
  type t = Global | Param of int [@@deriving yojson, equal]
end

module Record = struct
  type t =
    { c_type: Fir.Type.t
    ; c_id: Common.C_id.t
    ; mapped_to: Mapping.t
    ; initial_value: Fir.Constant.t option [@yojson.option] }
  [@@deriving accessors, yojson, equal]

  let mapped_to_global : t -> bool = function
    | {mapped_to= Global; _} ->
        true
    | _ ->
        false

  let mapped_to_param : t -> bool = function
    | {mapped_to= Param _; _} ->
        true
    | _ ->
        false
end

type t = Record.t Common.Scoped_map.t [@@deriving equal]

let lookup_and_require_global (map : t) ~(id : Common.Litmus_id.t) :
    Common.C_id.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind r = Common.Scoped_map.find_by_litmus_id map ~id in
    if Record.mapped_to_global r then Ok r.c_id
    else
      Or_error.error_s
        [%message
          "Litmus identifier was mapped to something other than a global \
           variable"
            ~id:(id : Common.Litmus_id.t)])

let lookup_and_require_param (map : t) ~(id : Common.Litmus_id.t) :
    Common.C_id.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind r = Common.Scoped_map.find_by_litmus_id map ~id in
    if Record.mapped_to_param r then Ok r.c_id
    else
      Or_error.error_s
        [%message
          "Litmus identifier was mapped to something other than a param"
            ~id:(id : Common.Litmus_id.t)])

let to_param_opt (lit_id : Common.Litmus_id.t) (rc : Record.t) :
    (int * (Common.Litmus_id.t * Record.t)) option =
  match rc.mapped_to with
  | Param k ->
      Some (k, (lit_id, rc))
  | Global ->
      None

let param_mapped_vars (vmap : t) :
    (Common.Litmus_id.t, Record.t) List.Assoc.t =
  (* Much of the complexity here is because of sorting by parameter position. *)
  vmap |> Common.Scoped_map.to_litmus_id_map |> Map.to_alist
  |> List.filter_map ~f:(fun (l, r) -> to_param_opt l r)
  |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
  |> List.map ~f:snd

let globally_mapped_vars : t -> (Common.Litmus_id.t, Record.t) List.Assoc.t =
  Tx.Fn.Compose_syntax.(
    Common.Scoped_map.filter ~f:Record.mapped_to_global
    >> Common.Scoped_map.to_litmus_id_map >> Map.to_alist)

let global_c_variables : t -> Set.M(Common.C_id).t =
  Common.Scoped_map.build_set
    (module Common.C_id)
    ~f:(fun _ r -> Record.(Option.some_if (mapped_to_global r) r.c_id))

module Json = Common.Scoped_map.Make_json (Record)

include (Json : module type of Json with type t := t)
