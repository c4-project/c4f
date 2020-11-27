(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test data over the 'global' configuration struct. *)

module Data : sig
  val global : Act_config.Global.t Lazy.t
  (** [global] evaluates to a sample global configuration. *)
end
