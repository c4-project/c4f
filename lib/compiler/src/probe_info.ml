(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  type t = {emits: Act_common.Id.t; version: string; name: string}
  [@@deriving compare, equal, sexp]
end

include M
include Comparable.Make (M)

let pp : t Fmt.t =
  Fmt.(
    record
      [ field "emits" (fun x -> x.emits) Act_common.Id.pp
      ; field "version" (fun x -> x.version) string
      ; field "name" (fun x -> x.name) string ])
