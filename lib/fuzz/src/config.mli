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

val module_map : Action.With_default_weight.t Map.M(Act_common.Id).t Lazy.t
(** [module_map] lazily evaluates to a map from action IDs to their modules. *)

(** {1 The configuration record} *)

(** {2 Constructing a fuzzer configuration} *)

type t [@@deriving sexp]
(** Opaque type of fuzzer configurations. *)

(** {2 Constructors} *)

val make :
     ?weights:(Act_common.Id.t, int) List.Assoc.t
  -> ?max_passes:int
  -> unit
  -> t
(** [make ?weights ?max_passes ()] constructs a fuzzer configuration with the
    given action weights table [weights] (defaulting to empty), and maximum
    number of passes [max_passes] (defaulting to a sensible number) *)

(** {2 Accessors} *)

val weights : t -> (Act_common.Id.t, int) List.Assoc.t
(** [weights cfg] gets the action weightings in [cfg]. *)

val max_passes : t -> int
(** [max_passes cfg] gets the configured maximum number of passes in [cfg]. *)

(** {2 Actions on a fuzzer configuration} *)

val make_pool : t -> Action.Pool.t Or_error.t
(** [make_pool config] tries to create the weighted action pool given the
    weights in [config]. This pool can then be queried for weight information
    or used to drive a random fuzzer session. *)

val summarise : t -> Action.Summary.t Map.M(Act_common.Id).t Or_error.t
(** [summarise config] tries to get the effective fuzzer weights setup using
    the weighting information in [config]. *)
