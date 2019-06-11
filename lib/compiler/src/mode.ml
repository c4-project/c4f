(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module M = struct
  type t = Assembly | Object [@@deriving enum]

  let table = [(Assembly, "assembly"); (Object, "object")]
end

include M
include Act_utils.Enum.Extend_table (M)
