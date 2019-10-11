(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for modules that convert from one type to the other. *)

open Base

(** Conversion with possible failure. *)
module type S_with_failure = sig
  type src
  (** Source type of the conversion. *)

  type dst
  (** Destination type of the conversion. *)

  val f : src -> dst Or_error.t
  (** [f s] tries to convert [s]. *)
end
