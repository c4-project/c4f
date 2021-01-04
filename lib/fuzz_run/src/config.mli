(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer configuration. *)

open Base
open Import

(** {1 The configuration record} *)

(** Opaque type of fuzzer configurations. *)
type t [@@deriving sexp]

(** {2 Constructors} *)

val make :
     ?weights:int Map.M(Common.Id).t
  -> ?params:int Map.M(Common.Id).t
  -> ?flags:Fuzz.Flag.t Map.M(Common.Id).t
  -> unit
  -> t
(** [make ?weights ?max_passes ()] constructs a fuzzer configuration with the
    given action weights table [weights] (defaulting to empty), and maximum
    number of passes [max_passes] (defaulting to a sensible number) *)

(** {2 Accessors} *)

(** {3 Raw configuration tables} *)

val weights : t -> int Map.M(Common.Id).t
(** [weights cfg] gets the user-provided action weightings in [cfg]. *)

val params : t -> int Map.M(Common.Id).t
(** [params cfg] gets the user-provided integer parameter settings in [cfg]. *)

val flags : t -> Fuzz.Flag.t Map.M(Common.Id).t
(** [flags cfg] gets the user-provided flag parameter settings in [cfg]. *)

(** {3 Making a parameter map} *)

val make_param_map : t -> Fuzz.Param_map.t
(** [make_param_map cfg] makes a parameter map from [cfg], ready for
    supplying to action payload and availability functions. *)

(** {2 Actions on a fuzzer configuration} *)

val make_pool :
     t
  -> Fuzz.Param_map.t
  -> random:Splittable_random.State.t
  -> Action_pool.t Or_error.t
(** [make_pool config params ~random] tries to create the weighted action
    pool given the weights in [config]. This pool can then be queried for
    weight information or used to drive a random fuzzer session. [params]
    should be taken from a previous call to {!make_param_map}; [random] will
    be used if [params] requests pool sampling. *)

(** {2 Making summaries} *)

(** A summary of each action in the config, and its calculated weight. *)
module Weight_summary : sig
  type elt := t

  type t = int Summary.t Map.M(Common.Id).t

  val summarise : elt -> t
  (** [summarise config] summarises the effective fuzzer weights setup using
      the weighting information in [config]. *)

  val pp : t Fmt.t
  (** [pp] pretty-prints a weight summary verbosely. *)

  val pp_terse : t Fmt.t
  (** [pp] pretty-prints a weight summary tersely. *)
end

(** A summary of each parameter and flag in the config, and its calculated
    value. *)
module Param_summary : sig
  type elt := t

  type t = Fuzz.Param_map.Value.t Summary.t Map.M(Common.Id).t

  val summarise : elt -> t Or_error.t
  (** [summarise config] summarises the effective fuzzer weights setup using
      the weighting information in [config]. *)

  val pp : t Fmt.t
  (** [pp] pretty-prints a param summary verbosely. *)

  val pp_terse : t Fmt.t
  (** [pp] pretty-prints a param summary tersely. *)
end
