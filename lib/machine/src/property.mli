(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-language for querying machine specifications, suitable for use in
    [Blang]. *)

open Core_kernel

(** [t] is the opaque type of property queries. *)
type t [@@deriving sexp]

val id : Act_common.Id.Property.t -> t
(** [id] constructs a query over a machine's ID. *)

val is_remote : t
(** [is_remote] constructs a query that asks if a machine is known to be
    remote. *)

val is_local : t
(** [is_local] constructs a query that asks if a machine is known to be
    local. *)

val eval : Spec.With_id.t -> t -> bool
(** [eval R reference property] evaluates [property] over [reference], with
    respect to module [R]. *)

val eval_b : Spec.With_id.t -> t Blang.t -> bool
(** [eval_b R reference expr] evaluates a [Blang] expression [expr] over
    [reference], with respect to module [R]. *)

include Act_common.Property_types.S with type t := t
