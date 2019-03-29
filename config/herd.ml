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

type t =
  { cmd: string [@default "herd7"] [@drop_if_default]
  ; c_model: string sexp_option
  ; asm_models: (Id.t, string) List.Assoc.t [@default []] [@drop_if_default]
  }
[@@deriving sexp, fields, make]

let make ?cmd ?c_model ?asm_models = make ?cmd ~c_model ?asm_models

module M : Program.S with type t := t = struct
  let argv _ = []

  let enabled _ = true

  let cmd = cmd

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp

  let default () = make ()
end

include M
