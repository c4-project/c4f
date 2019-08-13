(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer configuration. *)

open Base

(** {1 Constants} *)

val module_map : (module Action_types.S) Map.M(Act_common.Id).t Lazy.t
(** [module_map] lazily evaluates to a map from action IDs to their modules. *)

(** {1 The configuration record} *)

(** {2 Constructing a fuzzer configuration} *)

type t [@@deriving sexp]
(** Opaque type of fuzzer configurations. *)

(** {2 Constructors} *)

val make : ?weights:(Act_common.Id.t, int) List.Assoc.t -> unit -> t
(** [make ?weights ()] constructs a fuzzer configuration with the given
    action weights table (defaulting to empty). *)

(** {2 Actions on a fuzzer configuration} *)

val make_pool : t -> Action.Pool.t Or_error.t
(** [make_pool config] tries to create the weighted action pool given the
    weights in [config]. This pool can then be queried for weight
    information or used to drive a random fuzzer session. *)

val summarise : t -> Action.Summary.t Map.M(Act_common.Id).t Or_error.t
(** [summarise config] tries to get the effective fuzzer weights setup using
    the weighting information in [config]. *)
