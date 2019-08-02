(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Baseline interface of modules over configuration. *)
module type S = sig
  type t

  val defaults : t -> Default.t
  (** [defaults c] gets the defaults set specified by configuration [c]. *)

  val fuzz : t -> Act_fuzz.Config.t
  (** [fuzz c] gets the fuzzer config, if any, to use for configuration [c]. *)

  val machines : t -> Act_machine.Spec.Set.t
  (** [machines c] gets the set of all active machines in configuration [c]. *)
end
