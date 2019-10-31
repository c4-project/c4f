(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

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
