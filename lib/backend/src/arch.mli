(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = C | Assembly of Act_common.Id.t [@@deriving sexp, compare]

include Comparable.S with type t := t

(** {1 Predefined architectures}

    These are predominantly for testing. *)

val c : t
(** [c] is the C architecture, with no information about the underlying
    target. *)

val asm_x86 : t
(** [asm_x86] is the assembly architecture "x86". *)
