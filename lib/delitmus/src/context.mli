(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t
(** Opaque type of delitmus context. *)

(** {2 Constructors} *)

val make : 
     type_map:(Act_c.Mini.Type.t Map.M(Act_common.Litmus_id).t)
  -> aux:Aux.t
  -> local_inits:(int, (Act_common.C_id.t, Act_c.Mini.Constant.t) List.Assoc.t) List.Assoc.t
  -> t

(** {2 Variable lookups} *)

val lookup_type : t -> id:Act_common.Litmus_id.t -> Act_c.Mini.Type.t Or_error.t

val unmapped_litmus_ids : t -> Set.M(Act_common.Litmus_id).t
(** [unmapped_litmus_ids map] gets the set of litmus IDs that the delitmusifier
    did not map to global C variables in [map]. *)

val globally_mapped_litmus_ids : t -> Set.M(Act_common.Litmus_id).t

val lookup_and_require_global : t -> id:Act_common.Litmus_id.t -> Act_common.C_id.t Or_error.t

val lookup_initial_value : t -> id:Act_common.Litmus_id.t -> Act_c.Mini.Constant.t option
