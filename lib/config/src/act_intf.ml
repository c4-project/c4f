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

(** Baseline interface of modules over configuration. *)
module type S = sig
  module CSpec : Compiler.S_spec

  type t [@@deriving sexp]

  val cpp : t -> Cpp.t option
  (** [cpp c] gets the C preprocessor config, if any, to use for
      configuration [c]. *)

  val fuzz : t -> Fuzz.t option
  (** [fuzz c] gets the fuzzer config, if any, to use for configuration [c]. *)

  val herd : t -> Herd.t option
  (** [herd c] gets the Herd config, if any, to use for configuration [c]. *)

  val compilers : t -> CSpec.Set.t
  (** [compilers c] gets the set of all active compilers in configuration
      [c]. *)

  val machines : t -> Machine.Spec.Set.t
  (** [machines c] gets the set of all active machines in configuration [c]. *)

  val sanitiser_passes :
       t
    -> default:Set.M(Act_sanitiser.Pass_group).t
    -> Set.M(Act_sanitiser.Pass_group).t
  (** [sanitiser_passes c ~default] gets the set of requested sanitiser
      passes, given the default set [default] for the current context. *)
end
