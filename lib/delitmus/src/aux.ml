(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module M = struct
  type t =
    { litmus_header: Litmus_c.Header.t [@default Litmus.Header.empty]
    ; function_map: Function_map.t [@default Map.empty (module Common.C_id)]
    ; var_map: Var_map.t [@default Common.Scoped_map.empty] }
  [@@deriving accessors, make, equal, yojson]
end

include M

(* TODO(@MattWindsor91): validate litmus locations/etc against the variable
   map. *)
let empty : t = make ()

module Load : Plumbing.Loadable_types.S with type t := t =
  Plumbing.Loadable.Of_jsonable (M)

include Load

let pp : t Fmt.t =
  Fmt.(using yojson_of_t (Yojson.Safe.pretty_print ~std:false))
