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

let make_weight_alist (config : t) :
    (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t =
  let actions = Lazy.force Act_fuzz_actions.Table.actions in
  List.map ~f:(make_weight_pair config.weights) actions

let make_pool (config : t) (params : Fuzz.Param_map.t) :
    Action_pool.t Or_error.t =
  let weights = make_weight_alist config in
  Or_error.Let_syntax.(
    let%bind accept_rec_flag =
      Fuzz.Param_map.get_flag params
        ~id:Fuzz.Config_tables.accept_recommendation_flag
    in
    let%bind use_rec_flag =
      Fuzz.Param_map.get_flag params
        ~id:Fuzz.Config_tables.use_recommendation_flag
    in
    Action_pool.of_weighted_actions weights ~accept_rec_flag ~use_rec_flag)

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

module Weight_summary = struct
  open struct
    type elt = t
  end

  type t = int Summary.t Map.M(Common.Id).t

  let summarise_action ?(user_weight : int option)
      (w : Fuzz.Action.With_default_weight.t) : int Summary.t =
    let (module M) = Fuzz.Action.With_default_weight.action w in
    let default = Fuzz.Action.With_default_weight.default_weight w in
    let value = Summary.Adjusted.make ~default ?user:user_weight in
    let readme = Utils.My_string.format_for_readme (Lazy.force M.readme) in
    {value; readme}

  let summarise (config : elt) : t =
    List.fold
      (make_weight_alist config)
      ~init:(Map.empty (module Common.Id))
      ~f:
        (fun map
             ((action, user_weight) :
               Fuzz.Action.With_default_weight.t * int option) ->
        Map.set map
          ~key:(Fuzz.Action.With_default_weight.name action)
          ~data:(summarise_action action ?user_weight))

  let pp_weight (f : Formatter.t) (k : int) : unit =
    ( match k with
    | 0 ->
        Fmt.(styled (`Fg `Red) (any "disabled"))
    | _ ->
        Fmt.(styled (`Fg `Green) (int ++ any "x")) )
      f k

  let pp : t Fmt.t = Summary.pp_map pp_weight "Weight"

  let pp_terse : t Fmt.t = Summary.pp_map_terse pp_weight
end

module Param_summary = struct
  open struct
    type elt = t

    type s = Fuzz.Param_map.Value.t Summary.t
  end

  type t = s Map.M(Common.Id).t

  let get_adjusted (pm : Fuzz.Param_map.t)
      (original : Fuzz.Param_map.Value.t) (id : Common.Id.t) :
      Fuzz.Param_map.Value.t Summary.Adjusted.t Or_error.t =
    Or_error.Let_syntax.(
      let%map actual = Fuzz.Param_map.get pm ~id in
      (* TODO(@MattWindsor91): it'd be nice to have a better idea of when
         things have been overridden. *)
      if Fuzz.Param_map.Value.equal original actual then
        Summary.Adjusted.Not_adjusted original
      else Adjusted {original; actual})

  let summarise_elt (lift : 'a -> Fuzz.Param_map.Value.t)
      (pm : Fuzz.Param_map.t) (id : Common.Id.t)
      (spec : 'a Fuzz.Param_spec.t) : s Or_error.t =
    let default = lift (Fuzz.Param_spec.default spec) in
    let readme = Fuzz.Param_spec.description spec in
    Or_error.Let_syntax.(
      let%map value = get_adjusted pm default id in
      {Summary.value; readme})

  let summarise_elts (lift : 'a -> Fuzz.Param_map.Value.t)
      (pm : Fuzz.Param_map.t)
      (table : 'a Fuzz.Param_spec.t Map.M(Common.Id).t Lazy.t) :
      (Common.Id.t, s) List.Assoc.t Or_error.t =
    table |> Lazy.force |> Map.to_alist
    |> List.map ~f:(fun (k, v) ->
           Or_error.(summarise_elt lift pm k v >>| fun v' -> (k, v')))
    |> Or_error.combine_errors

  let summarise (cfg : elt) : t Or_error.t =
    let param_map = make_param_map cfg in
    Or_error.Let_syntax.(
      let%bind flag_summaries =
        summarise_elts
          (fun x -> Flag x)
          param_map Fuzz.Config_tables.flag_map
      in
      let%bind param_summaries =
        summarise_elts
          (fun x -> Param x)
          param_map Fuzz.Config_tables.param_map
      in
      Map.of_alist_or_error
        (module Common.Id)
        (flag_summaries @ param_summaries))

  let pp : t Fmt.t = Summary.pp_map Fuzz.Param_map.Value.pp "Value"

  let pp_terse : t Fmt.t = Summary.pp_map_terse Fuzz.Param_map.Value.pp
end
