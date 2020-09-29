(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t = Fuzz.Action.With_default_weight.t Utils.Weighted_list.t

let of_weighted_actions
    (weighted_actions :
      (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t) :
    t Or_error.t =
  (* TODO(@MattWindsor91): ideally we shouldn't lose whether the weight was
     overridden or not, even if the final weight equals the default one. *)
  weighted_actions
  |> List.map ~f:(fun (act, override) ->
         ( act
         , Option.value override
             ~default:(Fuzz.Action.With_default_weight.default_weight act) ))
  |> Utils.Weighted_list.from_alist

let summarise : t -> Fuzz.Action.Summary.t Map.M(Common.Id).t =
  Utils.Weighted_list.fold
    ~init:(Map.empty (module Common.Id))
    ~f:(fun map (action : Fuzz.Action.With_default_weight.t) weight ->
      (* TODO(@MattWindsor91): see TODO above, as it pertains to this bit
         too. *)
      let user_weight =
        if weight = Fuzz.Action.With_default_weight.default_weight action
        then None
        else Some weight
      in
      Map.set map
        ~key:(Fuzz.Action.With_default_weight.name action)
        ~data:(Fuzz.Action.Summary.of_action action ?user_weight))

let remove (table : t) (module C : Fuzz.Action_types.S) : t Or_error.t =
  (* TODO(@MattWindsor91): this is quite inefficient. *)
  Utils.Weighted_list.adjust_weights table ~f:(fun a' w ->
      if Common.Id.equal C.name (Fuzz.Action.With_default_weight.name a')
      then 0
      else w)

let pick (table : t) ~(random : Splittable_random.State.t) :
    (t * Fuzz.Action.t) Or_error.t =
  Or_error.Let_syntax.(
    let%bind w = Utils.Weighted_list.sample table ~random in
    let action = Fuzz.Action.With_default_weight.action w in
    let%map table' = remove table action in
    (table', action))
