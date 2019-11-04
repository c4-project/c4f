(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common

(** Configuration needed to find and execute Herd. *)

type t

(** Simulator specifications are specifications. *)
include
  Act_common.Spec.S
    with type t := t
     and type With_id.t = t Act_common.Spec.With_id.t

(** {2 Constructors} *)

val make :
     ?cmd:string
  -> ?argv:string list
  -> ?c_model:string
  -> ?asm_models:(Id.t, string) List.Assoc.t
  -> style:Id.t
  -> unit
  -> t
(** [make ?cmd ?argv ~c_model ~asm_models ~style ()] creates a backend spec
    with the given fields. *)

(** {2 Accessors} *)

val cmd : t -> string
(** [cmd cfg] gets the configured command. *)

val argv : t -> string list
(** [argv cfg] gets the configured arguments. *)

val c_model : t -> string option
(** [c_models cfg] gets the configured C model overrides in [cfg], if one
    exists. *)

val asm_models : t -> (Id.t, string) List.Assoc.t
(** [asm_models cfg] gets the list of configured assembly model overrides in
    [cfg]. Each override maps an architecture, represented by its config ID,
    to a path. *)

val style : t -> Id.t
(** [style cfg] gets the 'style' of simulator: for example, 'litmus' or
    'herd'. *)
