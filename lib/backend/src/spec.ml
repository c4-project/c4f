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
    { cmd: string [@default "herd7"] [@drop_if_default]
    ; argv: string list [@sexp.list]
    ; c_model: string option [@sexp.option]
    ; asm_models: (Ac.Id.t * string) list [@default []] [@drop_if_default]
    ; style: Ac.Id.t }
  [@@deriving sexp, fields, make, equal]

  let enabled _ = true

  let cmd = cmd

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end

(* There is a _lot_ of module acrobatics here to make sure we only define
   't' once.

   TODO(@MattWindsor91): when 4.08 arrives, check to see if this is really
   necessary. *)
include M

module Spec_common = struct
  type t = M.t [@@deriving sexp, equal]

  let is_enabled = enabled

  let type_name = "sim"

  let pp_summary : t Fmt.t = Fmt.always "TODO"

  let pp : t Fmt.t = Fmt.always "TODO"
end

module Spec = Ac.Spec.Make (struct
  include Spec_common
  module With_id = Ac.Spec.Make_with_id (Spec_common)
end)

include (Spec : module type of Spec with type t := M.t)
