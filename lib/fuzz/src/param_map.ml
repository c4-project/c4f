(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { params: int Map.M(Act_common.Id).t
        [@default Map.empty (module Act_common.Id)]
  ; flags: Flag.t Map.M(Act_common.Id).t
        [@default Map.empty (module Act_common.Id)] }
[@@deriving sexp, fields, make]
(** Opaque type of parameter maps. *)

let find (m : 'a Map.M(Act_common.Id).t) ~(map_name : string)
    ~(id : Act_common.Id.t) : 'a Or_error.t =
  Act_utils.My_map.find_or_error ~sexp_of_key:[%sexp_of: Act_common.Id.t]
    ~map_name m id

let get_param (pmap : t) ~(id : Act_common.Id.t) : int Or_error.t =
  find (params pmap) ~id ~map_name:"parameter map"

let get_flag (pmap : t) ~(id : Act_common.Id.t) : Flag.t Or_error.t =
  find (flags pmap) ~id ~map_name:"flag map"

let get_action_cap : t -> int Or_error.t =
  get_param ~id:(Act_common.Id.of_string "cap.action")

let get_thread_cap : t -> int Or_error.t =
  get_param ~id:(Act_common.Id.of_string "cap.thread")
