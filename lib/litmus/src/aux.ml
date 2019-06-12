(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Ac = Act_common

type 'const t =
  { locations: Ac.C_id.t list option
  ; init: (Ac.C_id.t, 'const) List.Assoc.t [@default []]
  ; postcondition: 'const Ast_base.Postcondition.t option }
[@@deriving fields, make]
