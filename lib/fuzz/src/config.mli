(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer configuration. *)

open Base

(** {1 The configuration record} *)

(** {2 Constructing a fuzzer configuration} *)

(** Opaque type of fuzzer configurations. *)
type t [@@deriving sexp]

(** {2 Constructors} *)

val make :
     ?weights:int Map.M(Act_common.Id).t
  -> ?params:int Map.M(Act_common.Id).t
  -> ?flags:Flag.t Map.M(Act_common.Id).t
  -> unit
  -> t
(** [make ?weights ?max_passes ()] constructs a fuzzer configuration with the
    given action weights table [weights] (defaulting to empty), and maximum
    number of passes [max_passes] (defaulting to a sensible number) *)

(** {2 Accessors} *)

(** {3 Raw configuration tables} *)

val weights : t -> int Map.M(Act_common.Id).t
(** [weights cfg] gets the user-provided action weightings in [cfg]. *)

val params : t -> int Map.M(Act_common.Id).t
(** [params cfg] gets the user-provided integer parameter settings in [cfg]. *)

val flags : t -> Flag.t Map.M(Act_common.Id).t
(** [flags cfg] gets the user-provided flag parameter settings in [cfg]. *)

(** {3 Making a parameter map} *)

val make_param_map : t -> Param_map.t
(** [make_param_map cfg] makes a parameter map from [cfg], ready for
    supplying to action payload and availability functions. *)

(** {2 Actions on a fuzzer configuration} *)

val make_pool : t -> Action.Pool.t Or_error.t
(** [make_pool config] tries to create the weighted action pool given the
    weights in [config]. This pool can then be queried for weight information
    or used to drive a random fuzzer session. *)

val summarise : t -> Action.Summary.t Map.M(Act_common.Id).t Or_error.t
(** [summarise config] tries to get the effective fuzzer weights setup using
    the weighting information in [config]. *)
