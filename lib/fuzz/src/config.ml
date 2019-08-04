(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { weights: (Act_common.Id.t, int) List.Assoc.t
        [@default []] [@drop_if_default] }
[@@deriving sexp, fields, make]

let modules : (module Action_types.S) list Lazy.t =
  lazy
    [ (module Var_actions.Make_global : Action_types.S)
    ; (module Store_actions.Int : Action_types.S)
    ; (module Program_actions.Make_empty : Action_types.S) ]

let make_weight_pair (weight_overrides : int Map.M(Act_common.Id).t)
    (module M : Action_types.S) : (module Action_types.S) * int =
  let weight =
    M.name |> Map.find weight_overrides
    |> Option.value ~default:M.default_weight
  in
  ((module M : Action_types.S), weight)

let make_weight_alist (actions : (module Action_types.S) list)
    (weight_overrides : int Map.M(Act_common.Id).t) :
    ((module Action_types.S), int) List.Assoc.t =
  List.map ~f:(make_weight_pair weight_overrides) actions

let make_pool (config : t) : Action.Pool.t Or_error.t =
  let actions = Lazy.force modules in
  let weight_overrides_alist = weights config in
  Or_error.Let_syntax.(
    let%bind weight_overrides =
      Map.of_alist_or_error (module Act_common.Id) weight_overrides_alist
    in
    let weights = make_weight_alist actions weight_overrides in
    Action.Pool.of_weighted_actions weights)

let summarise (cfg : t) : Action.Summary.t Map.M(Act_common.Id).t Or_error.t
    =
  Or_error.(cfg |> make_pool >>| Action.Pool.summarise)
