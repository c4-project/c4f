(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Sample compiler specifications and suchlike for tests. *)

module Spec_sets : sig
  val gcc_spec : Act_compiler.Spec.t Lazy.t
  (** [gcc_spec] evaluates to an example GCC compiler spec. *)

  val disabled_gcc_spec : Act_compiler.Spec.t Lazy.t
  (** [disabled_gcc_spec] evaluates to an example GCC compiler spec that has
      been disabled in the configuration. *)

  val single_gcc_compiler : Act_compiler.Spec.Set.t Lazy.t
  (** [single_gcc_compiler] evaluates to an example compiler specset
      containing only [gcc_spec]. *)
end
