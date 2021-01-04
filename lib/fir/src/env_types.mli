(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: module signatures for variable environments *)

(** Type of modules containing typing/known-value environments. *)
module type S = sig
  val env : Env.t
  (** [env] is a variable environment. *)
end

(** Type of modules containing both environments and flags about the current
    context; useful for expression generation. *)
module type S_with_flags = sig
  include S

  val consume_enabled : bool
  (** [consume_enabled] checks whether the 'consume' memory order may be
      generated in this context. *)

  val atomics_enabled : bool
  (** [atomics_enabled] checks whether atomic actions may be generated in
      this context. *)
end
