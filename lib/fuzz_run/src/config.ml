(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module F = Act_fuzz
end

type t =
  { weights: int Map.M(Ac.Id).t
        [@default Map.empty (module Ac.Id)] [@drop_if_default]
  ; params: int Map.M(Ac.Id).t
        [@default Map.empty (module Ac.Id)] [@drop_if_default]
  ; flags: F.Flag.t Map.M(Ac.Id).t
        [@default Map.empty (module Ac.Id)] [@drop_if_default] }
[@@deriving sexp, fields, make]

let make_weight_pair (weight_overrides : int Map.M(Ac.Id).t)
    (action : F.Action.With_default_weight.t) :
    F.Action.With_default_weight.t * int option =
  let weight =
    Map.find weight_overrides (F.Action.With_default_weight.name action)
  in
  (action, weight)

let make_weight_alist (actions : F.Action.With_default_weight.t list)
    (weight_overrides : int Map.M(Ac.Id).t) :
    (F.Action.With_default_weight.t, int option) List.Assoc.t =
  List.map ~f:(make_weight_pair weight_overrides) actions

let make_pool (config : t) : F.Action.Pool.t Or_error.t =
  let actions = Lazy.force Act_fuzz_actions.Table.actions in
  let weight_overrides = weights config in
  let weights = make_weight_alist actions weight_overrides in
  F.Action.Pool.of_weighted_actions weights

let make_merged_param_map (type v)
    (specs : v F.Param_spec.t Map.M(Ac.Id).t Lazy.t)
    (user_provided : v Map.M(Ac.Id).t) : v Map.M(Ac.Id).t =
  let defaults = specs |> Lazy.force |> Map.map ~f:F.Param_spec.default in
  (* Yeet any values that are user-provided but don't correspond to actual
     parameters.

     TODO(@MattWindsor91): error on these? *)
  Map.merge defaults user_provided ~f:(fun ~key ->
      ignore key ;
      function `Left v | `Both (_, v) -> Some v | `Right _ -> None)

let make_param_map (config : t) : F.Param_map.t =
  let params =
    make_merged_param_map F.Config_tables.param_map (params config)
  in
  let flags =
    make_merged_param_map F.Config_tables.flag_map (flags config)
  in
  F.Param_map.make ~params ~flags ()

let summarise (cfg : t) : F.Action.Summary.t Map.M(Ac.Id).t Or_error.t =
  Or_error.(cfg |> make_pool >>| F.Action.Pool.summarise)
