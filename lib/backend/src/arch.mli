(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Type of architecture specifiers for ACT backends. *)
type t =
  | C of {underlying_arch: Act_common.Id.t option}
      (** Use the backend's C support.

          Some backends require a hint as to the architecture to which the C
          will be compiled. This is the optional field [underlying_arch]. *)
  | Assembly of Act_common.Id.t
      (** Use the backend's assembly support for the given architecture. *)
[@@deriving sexp, compare]

include Comparable.S with type t := t

(** {1 Predefined architectures}

    These are predominantly for testing. *)

val c : t
(** [c] is the C architecture, with no information about the underlying
    target. *)

val c_x86 : t
(** [c] is the C architecture, with underlying architecture "x86". *)

val asm_x86 : t
(** [asm_x86] is the assembly architecture "x86". *)
