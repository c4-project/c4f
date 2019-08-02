(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [Blang]-based property interface for filtering compilers. *)

open Core_kernel

type t [@@deriving sexp]
(** [t] is the opaque type of property queries. *)

val id : Act_common.Id.Property.t -> t
(** [id] constructs a query over a compiler's ID. *)

val eval : Spec.With_id.t -> t -> bool
(** [eval cspec property] evaluates [property] over [cspec]. *)

val eval_b : Spec.With_id.t -> t Blang.t -> bool
(** [eval_b cspec expr] evaluates a [Blang] expression [expr] over [cspec]. *)

include Act_common.Property_types.S with type t := t
