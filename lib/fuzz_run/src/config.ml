(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t =
  { weights: int Map.M(Common.Id).t
        [@default Map.empty (module Common.Id)] [@drop_if_default]
  ; params: int Map.M(Common.Id).t
        [@default Map.empty (module Common.Id)] [@drop_if_default]
  ; flags: Fuzz.Flag.t Map.M(Common.Id).t
        [@default Map.empty (module Common.Id)] [@drop_if_default] }
[@@deriving sexp, fields, make]

let make_weight_pair (weight_overrides : int Map.M(Common.Id).t)
    (action : Fuzz.Action.With_default_weight.t) :
    Fuzz.Action.With_default_weight.t * int option =
  let weight =
    Map.find weight_overrides (Fuzz.Action.With_default_weight.name action)
  in
  (action, weight)

let make_weight_alist (actions : Fuzz.Action.With_default_weight.t list)
    (weight_overrides : int Map.M(Common.Id).t) :
    (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t =
  List.map ~f:(make_weight_pair weight_overrides) actions

let make_pool (config : t) : Fuzz.Action.Pool.t Or_error.t =
  let actions = Lazy.force Act_fuzz_actions.Table.actions in
  let weight_overrides = weights config in
  let weights = make_weight_alist actions weight_overrides in
  Fuzz.Action.Pool.of_weighted_actions weights

let make_merged_param_map (type v)
    (specs : v Fuzz.Param_spec.t Map.M(Common.Id).t Lazy.t)
    (user_provided : v Map.M(Common.Id).t) : v Map.M(Common.Id).t =
  let defaults = specs |> Lazy.force |> Map.map ~f:Fuzz.Param_spec.default in
  (* Yeet any values that are user-provided but don't correspond to actual
     parameters.

     TODO(@MattWindsor91): error on these? *)
  Map.merge defaults user_provided ~f:(fun ~key ->
      ignore key ;
      function `Left v | `Both (_, v) -> Some v | `Right _ -> None)

let make_param_map (config : t) : Fuzz.Param_map.t =
  let params =
    make_merged_param_map Fuzz.Config_tables.param_map (params config)
  in
  let flags =
    make_merged_param_map Fuzz.Config_tables.flag_map (flags config)
  in
  Fuzz.Param_map.make ~params ~flags ()

let summarise (cfg : t) : Fuzz.Action.Summary.t Map.M(Common.Id).t Or_error.t
    =
  Or_error.(cfg |> make_pool >>| Fuzz.Action.Pool.summarise)
