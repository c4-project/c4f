(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module M = struct
  open Caml

  type t =
    { litmus_header: Act_c_mini.Litmus_header.t
          [@default Act_litmus.Header.empty]
    ; var_map: Var_map.t
    ; num_threads: int }
  [@@deriving make, fields, equal, yojson]
end

include M

(* Using Base above this line interferes with yojson! *)
open Base

(* TODO(@MattWindsor91): validate litmus locations/etc against the variable
   map. *)

let symbols (aux : t) : string list =
  aux |> var_map |> Var_map.global_c_variables |> Set.to_list
  |> List.map ~f:Act_common.C_id.to_string

let empty : t =
  { litmus_header= Act_litmus.Header.empty
  ; var_map= Act_common.Scoped_map.empty
  ; num_threads= 0 }

module Load : Plumbing.Loadable_types.S with type t := t =
  Plumbing.Loadable.Of_jsonable (M)

include Load
