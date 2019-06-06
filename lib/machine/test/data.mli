(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Sample machine specifications and suchlike for tests. *)

module Spec_sets : sig
  val single_local_machine : Act_machine.Spec.Set.t Lazy.t
  (** [single_local_machine] evaluates to a sample machine spec set
      containing one locally-run machine. *)
end
