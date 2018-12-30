(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Baseline interface for program interaction. *)
module type S_program = sig
  type t
  (** The type of program configuration. *)

  val default : unit -> t
  (** [default ()] gets the default configuration for this program. *)

  val enabled : t -> bool
  (** [enabled cfg] gets whether this program is enabled, according to [cfg]. *)

  val cmd     : t -> string
  (** [cmd cfg] gets the configured command. *)

  val argv    : t -> string list
  (** [argv cfg] gets the configured arguments. *)
end

(** Baseline interface of modules over configuration. *)
module type S = sig
  module CSpec : Compiler.S_spec

  module Cpp : S_program

  type t [@@deriving sexp]

  val cpp : t -> Cpp.t option
  (** [cpp c] gets the C preprocessor config, if any, to use for configuration
      [c]. *)

  val herd : t -> Herd.Config.t option
  (** [herd c] gets the Herd config, if any, to use for configuration
      [c]. *)

  val compilers : t -> CSpec.Set.t
  (** [compilers c] gets the set of all active compilers in
      configuration [c]. *)

  val machines : t -> Machine.Spec.Set.t
  (** [machines c] gets the set of all active machines in
      configuration [c]. *)

  val sanitiser_passes
    :  t
    -> default:Sanitiser_pass.Set.t
    -> Sanitiser_pass.Set.t
  (** [sanitiser_passes c ~default] gets the set of requested
      sanitiser passes, given the default set [default] for the
      current context. *)
end
