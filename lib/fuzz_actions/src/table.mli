(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The fuzzer action tables. *)

open Base

val actions : C4f_fuzz.Action.With_default_weight.t list Lazy.t
(** [actions] is a listing of all actions with their default weights. *)

val action_map :
  C4f_fuzz.Action.With_default_weight.t Map.M(C4f_common.Id).t Lazy.t
(** [action_map] lazily evaluates to a map from action IDs to their actions.
    It is, effectively, a rearrangement of the data available in [actions]. *)
