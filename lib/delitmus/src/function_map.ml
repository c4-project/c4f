(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Import

module Record = struct
  type t = {tid: int option [@default None]; c_id: Common.C_id.t}
  [@@deriving yojson, equal, accessors]

  let is_thread_body ({tid; _} : t) : bool = Option.is_some tid
end

include Plumbing.Jsonable.Make_map (Common.C_id) (Record)

let equal : t -> t -> bool = Map.equal Record.equal

let num_threads : t -> int = Map.count ~f:Record.is_thread_body
