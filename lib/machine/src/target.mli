(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of target specifiers for jobs that can accept both C files and
    assembly. *)

open Base

type t = [`Spec of Qualified.Compiler.t | `Arch of Act_common.Id.t]
(** [t] is either a machine-qualified compiler specification, or a raw
    architecture. *)

val arch : t -> Act_common.Id.t
(** [arch_of_target target] gets the architecture ID associated with
    [target]. *)

val ensure_spec : t -> Qualified.Compiler.t Or_error.t
(** [ensure_spec target] extracts a compiler spec from [target], failing if
    it is a raw architecture. *)
