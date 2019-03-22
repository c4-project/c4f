(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base

module type Basic = Asm_job_intf.Runner_deps (* for now *)

(** Signature of constructed litmusifier modules. *)
module type S = sig
  (** Type of config. *)
  type conf

  (** Type of formatting directives. *)
  type fmt

  (** The AST module for the litmus test language we're targeting. *)
  module Litmus : Litmus.Ast.S

  (** The sanitiser module whose output we're litmusifying. *)
  module Sanitiser : Sanitiser.S

  (** [make ~config ~redirects ~name ~programs] produces a
  *)
  val make
    :  config:conf
    -> redirects:Sanitiser.Redirect.t
    -> name:string
    -> programs:Sanitiser.Output.Program.t list
    -> Litmus.Validated.t Or_error.t

  (** [pp_litmus config] is the litmus test pretty-printer
      matching the configuration [config. *)
  val pp_litmus : fmt -> Litmus.Validated.t Fmt.t
end
