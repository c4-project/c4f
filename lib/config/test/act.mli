(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test data over the 'act' configuration struct. *)

module Data : sig
  val act : Act_config.Act.t Lazy.t
  (** [act] evaluates to a sample ACT configuration. *)
end
