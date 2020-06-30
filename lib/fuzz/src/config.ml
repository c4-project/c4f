(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { weights: int Map.M(Act_common.Id).t
        [@default Map.empty (module Act_common.Id)] [@drop_if_default]
  ; params: int Map.M(Act_common.Id).t
        [@default Map.empty (module Act_common.Id)] [@drop_if_default]
  ; flags: Flag.t Map.M(Act_common.Id).t
        [@default Map.empty (module Act_common.Id)] [@drop_if_default] }
[@@deriving sexp, fields, make]

let make_weight_pair (weight_overrides : int Map.M(Act_common.Id).t)
    (action : Action.With_default_weight.t) :
    Action.With_default_weight.t * int option =
  let weight =
    Map.find weight_overrides (Action.With_default_weight.name action)
  in
  (action, weight)

let make_weight_alist (actions : Action.With_default_weight.t list)
    (weight_overrides : int Map.M(Act_common.Id).t) :
    (Action.With_default_weight.t, int option) List.Assoc.t =
  List.map ~f:(make_weight_pair weight_overrides) actions

let make_pool (config : t) : Action.Pool.t Or_error.t =
  let actions = Lazy.force Config_tables.actions in
  let weight_overrides = weights config in
  let weights = make_weight_alist actions weight_overrides in
  Action.Pool.of_weighted_actions weights

let make_merged_param_map (type v)
    (specs : v Param_spec.t Map.M(Act_common.Id).t Lazy.t)
    (user_provided : v Map.M(Act_common.Id).t) : v Map.M(Act_common.Id).t =
  let defaults = specs |> Lazy.force |> Map.map ~f:Param_spec.default in
  (* Yeet any values that are user-provided but don't correspond to actual
     parameters.

     TODO(@MattWindsor91): error on these? *)
  Map.merge defaults user_provided ~f:(fun ~key ->
      ignore key ;
      function `Left v | `Both (_, v) -> Some v | `Right _ -> None )

let make_param_map (config : t) : Param_map.t =
  let params =
    make_merged_param_map Config_tables.param_map (params config)
  in
  let flags = make_merged_param_map Config_tables.flag_map (flags config) in
  Param_map.make ~params ~flags ()

let summarise (cfg : t) : Action.Summary.t Map.M(Act_common.Id).t Or_error.t
    =
  Or_error.(cfg |> make_pool >>| Action.Pool.summarise)
