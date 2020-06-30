(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Machine specifications. *)

open Base

include Spec_types.S with type via := Via.t

val make :
     ?enabled:bool
  -> ?via:Via.t
  -> ?backends:Act_backend.Spec.Set.t
  -> unit
  -> t
(** [make ?enabled ?via ?backends ()] creates a machine spec with the given
    fields.

    These fields are subject to change, and as such [make] is an unstable
    API. *)

(** [With_id] is an implementation of [Spec.With_id]. *)
module With_id :
  Act_common.Spec_types.S_with_id
    with type elt := t
     and type t = t Act_common.Spec.With_id.t

(** Machine specifications are specifications. *)
include Act_common.Spec.S with type t := t and module With_id := With_id
