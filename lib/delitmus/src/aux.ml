(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

type t =
  { litmus_aux: Act_c.Mini.Constant.t Act_litmus.Aux.t [@default Act_litmus.Aux.empty]
  ; var_map: Var_map.t }
[@@deriving make, fields, equal]

(* TODO(@MattWindsor91): validate litmus locations/etc against the
   variable map. *)

let symbols (aux : t) : string list =
  aux |> var_map
  |> Var_map.global_c_variables
  |> Set.to_list
  |> List.map ~f:Act_common.C_id.to_string

let empty : t =
  { litmus_aux= Act_litmus.Aux.empty
  ; var_map= Var_map.empty
  }

