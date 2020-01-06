(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various lazily-evaluated tables used to source information about fuzzer
    configurables. *)

open Base

val actions : Action.With_default_weight.t list Lazy.t
(** [actions] is a listing of all actions with their default weights. *)

val action_map : Action.With_default_weight.t Map.M(Act_common.Id).t Lazy.t
(** [module_map] lazily evaluates to a map from action IDs to their actions.
    It is, effectively, a rearrangement of the data available in [actions]. *)

val param_map : Param_spec.Int.t Map.M(Act_common.Id).t Lazy.t
(** [param_map] lazily evaluates to a map from integer parameter IDs to their
    specifications, including their default values. *)

val flag_map : Param_spec.Bool.t Map.M(Act_common.Id).t Lazy.t
(** [flag_map] lazily evaluates to a map from Boolean flag IDs to their
    specifications, including their default values. *)
