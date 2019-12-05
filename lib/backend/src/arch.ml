(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  type t =
    | C of {underlying_arch: Act_common.Id.t option}
    | Assembly of Act_common.Id.t
  [@@deriving sexp, compare]
end

include M
include Comparable.Make (M)

let c : t = C {underlying_arch= None}

let c_x86 : t = C {underlying_arch= Some (Act_common.Id.of_string "x86")}

let asm_x86 : t = Assembly (Act_common.Id.of_string "x86")
