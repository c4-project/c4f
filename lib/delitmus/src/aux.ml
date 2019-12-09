(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

module M = struct
  type t =
    { litmus_header: Act_c_mini.Litmus_header.t
          [@default Act_litmus.Header.empty]
    ; function_map: Function_map.t
                    [@default Map.empty (module Act_common.C_id)]
    ; var_map: Var_map.t
          [@default Act_common.Scoped_map.empty] }
  [@@deriving make, fields, equal, yojson]
end
include M

(* TODO(@MattWindsor91): validate litmus locations/etc against the variable
   map. *)

let symbols (aux : t) : string list =
  aux |> var_map |> Var_map.global_c_variables |> Set.to_list
  |> List.map ~f:Act_common.C_id.to_string

let empty : t = make ()

module Load : Plumbing.Loadable_types.S with type t := t =
  Plumbing.Loadable.Of_jsonable (M)

include Load

let pp : t Fmt.t =
  Fmt.(using yojson_of_t (Yojson.Safe.pretty_print ~std:false))
