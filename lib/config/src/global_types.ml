(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Baseline interface of modules over configuration. *)
module type S = sig
  type t

  val cpp : t -> Cpp.t option
  (** [cpp c] gets the C preprocessor config, if any, to use for
      configuration [c]. *)

  val defaults : t -> Ast.Default.t list

  val fuzz : t -> Fuzz.t option
  (** [fuzz c] gets the fuzzer config, if any, to use for configuration [c]. *)

  val machines : t -> Act_compiler.Machine_spec.Set.t
  (** [machines c] gets the set of all active machines in configuration [c]. *)
end
