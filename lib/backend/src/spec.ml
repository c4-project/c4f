(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

module M = struct
  type t =
    { argv: string list [@sexp.list]
    ; c_model: string option [@sexp.option]
    ; asm_models: (Ac.Id.t * string) list [@default []] [@drop_if_default]
    ; cmd: string
    ; style: Ac.Id.t }
  [@@deriving sexp, fields, make, equal]

  let enabled _ = true

  let cmd = cmd

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end

(* There is a _lot_ of module acrobatics here to make sure we only define 't'
   once.

   TODO(@MattWindsor91): when 4.08 arrives, check to see if this is really
   necessary. *)
include M

module Spec_common = struct
  type t = M.t [@@deriving sexp, equal]

  let is_enabled = enabled

  let type_name = "backend"

  let cmd_line (s : t) : string list = s.cmd :: s.argv

  let pp =
    Fmt.(
      vbox
        (record
           [ field "Enabled" is_enabled bool
           ; field "Style" style Ac.Id.pp
           ; field "Command" cmd_line (hbox (list ~sep:sp string)) ]))

  let pp_summary =
    Fmt.(
      concat ~sep:sp
        [using style Ac.Id.pp; using is_enabled Ac.Spec.pp_enabled_summary])
end

module Spec = Ac.Spec.Make (struct
  include Spec_common
  module With_id = Ac.Spec.Make_with_id (Spec_common)
end)

include (Spec : module type of Spec with type t := M.t)
