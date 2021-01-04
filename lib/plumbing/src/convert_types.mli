(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for modules that convert from one type to the other. *)

open Base

(** Conversion with possible failure. *)
module type S_with_failure = sig
  (** Source type of the conversion. *)
  type src

  (** Destination type of the conversion. *)
  type dst

  val f : src -> dst Or_error.t
  (** [f s] tries to convert [s]. *)
end
