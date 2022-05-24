(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** A map containing fuzzer parameters (other than actions). *)

(** Opaque type of parameter maps. *)
type t [@@deriving sexp]

(** {1 Constructors} *)

val make :
     ?params:int Map.M(Common.Id).t
  -> ?flags:Flag.t Map.M(Common.Id).t
  -> unit
  -> t
(** [make ~params ~flags] builds a parameter map from integer parameters
    [params] and flag parameters [flags]. It does not insert any default
    mappings into the maps. *)

(** {1 Resolving parameters and flags} *)

(** Variant representation of parameters and flags *)
module Value : sig
  type t = Param of int | Flag of Flag.t [@@deriving equal]

  include Pretty_printer.S with type t := t
end

val get : t -> id:Common.Id.t -> Value.t Or_error.t
(** [get map ~id] tries to get the parameter or flag with ID [id] from map
    [map]. It fails if no such parameter or flag exists. *)

val get_param : t -> id:Common.Id.t -> int Or_error.t
(** [get_param map ~id] tries to get the integer parameter with ID [id] from
    map [map]. It fails if no such parameter exists. *)

val get_flag : t -> id:Common.Id.t -> Flag.t Or_error.t
(** [get_flag map ~id] tries to get the flag with ID [id] from map [map]. It
    fails if no such flag exists. The flag returned will need a random number
    generator to evaluate into a boolean. *)

val get_action_cap : t -> random:Splittable_random.State.t -> int Or_error.t
(** [get_action_cap map ~random] randomly chooses the number of actions that
    an instance of the randomised fuzzer runner should generate, given the
    caps and extra flag distribution in [map] and the randomiser [random]. *)

(** See {!Config_tables} for well-known param and flag IDs. *)
