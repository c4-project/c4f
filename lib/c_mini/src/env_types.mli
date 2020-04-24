(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** C-mini: module signatures for variable environments

These exist because of the sheer number of functors that accept {!Env} types. *)

(** Type of modules containing typing/known-value environments. *)
module type S = sig
  val env : Env.t
  (** [env] is a variable environment. *)
end
