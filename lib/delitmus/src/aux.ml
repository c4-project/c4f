(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Tx = Travesty_base_exts

(* TODO(@MattWindsor91): perhaps move this outwards. *)
module C_aux = struct
  type t = Act_c_lang.Ast_basic.Constant.t Act_litmus.Aux.t
  [@@deriving equal]

  module J : Plumbing.Jsonable_types.S with type t := t =
  Act_litmus.Aux.Json (struct
    include Act_c_lang.Ast_basic.Constant

    let parse_post_string = Act_c_lang.Frontend.Litmus_post.load_from_string
  end)

  include J
end

(* Nasty hack to get the derivations for [t] to work. *)
let equal_int = Base.Int.equal

module M = struct
  type t =
    { litmus_aux: C_aux.t [@default Act_litmus.Aux.empty]
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
  {litmus_aux= Act_litmus.Aux.empty; var_map= Var_map.empty; num_threads= 0}

module Load : Plumbing.Loadable_types.S with type t := t =
  Plumbing.Loadable.Of_jsonable (M)

include Load
