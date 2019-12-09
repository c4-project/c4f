(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Record = struct
  type t = {is_thread_body: bool [@default false]; c_id: Act_common.C_id.t}
  [@@deriving yojson, equal, make, fields]
end

include Plumbing.Jsonable.Make_map (Act_common.C_id) (Record)

let equal : t -> t -> bool = Map.equal Record.equal

let num_threads : t -> int = Map.count ~f:Record.is_thread_body
