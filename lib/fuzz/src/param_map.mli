(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** A map containing fuzzer parameters (other than actions). *)

(** Opaque type of parameter maps. *)
type t [@@deriving sexp]

(** {1 Constructors} *)

val make :
     ?params:int Map.M(Act_common.Id).t
  -> ?flags:Flag.t Map.M(Act_common.Id).t
  -> unit
  -> t
(** [make ~params ~flags] builds a parameter map from integer parameters
    [params] and flag parameters [flags]. It does not insert any default
    mappings into the maps. *)

(** {1 Resolving parameters and flags} *)

val get_param : t -> id:Act_common.Id.t -> int Or_error.t
(** [get_param map ~id] tries to get the integer parameter with ID [id] from
    map [map]. It fails if no such parameter exists. *)

val get_flag : t -> id:Act_common.Id.t -> Flag.t Or_error.t
(** [get_flag map ~id] tries to get the flag with ID [id] from map [map]. It
    fails if no such flag exists. The flag returned will need a random number
    generator to evaluate into a boolean. *)

(** {2 Well-known parameters and flags} *)

val get_action_cap : t -> int Or_error.t
(** [get_action_cap map] tries to get the action cap configured in [map]. It
    fails if one doesn't exist. *)

val get_thread_cap : t -> int Or_error.t
(** [get_thread_cap map] tries to get the thread cap configured in [map]. It
    fails if one doesn't exist. *)
