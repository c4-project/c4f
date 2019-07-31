(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module type S =
  Plumbing.Filter_types.S with type aux_i = Arch.t and type aux_o = unit

module Make_error (B : sig
  val error : Error.t
end) : S