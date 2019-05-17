(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Signature of constructed litmusifier modules. *)
module type S = sig
  (** Type of config. *)
  type config

  (** Type of formatting directives. *)
  type fmt

  (** Type of incoming programs. *)
  type program

  (** The AST module for the litmus test language we're targeting. *)
  module Litmus : Act_litmus.Ast.S

  (** The redirect map for this litmusifier's input symbols. *)
  module Redirect : Act_common.Redirect_map.S

  val make :
       config:config
    -> redirects:Redirect.t
    -> name:string
    -> programs:program list
    -> Litmus.Validated.t Or_error.t
  (** [make ~config ~redirects ~name ~programs] litmusifies a list of
      programs [programs], with test name [name], redirect map [redirects],
      and configuration [config]. *)

  val print_litmus :
    fmt -> Stdio.Out_channel.t -> Litmus.Validated.t -> unit
  (** [print_litmus fmt oc ast] is the litmus test printer matching the
      configuration [fmt]. *)

  (** [Filter] is the litmusifier packaged up as an assembly job runner, ie
      a filter accepting an assembly job and outputting a standard job
      output. *)
  module Filter : Runner.S with type cfg = config
end
