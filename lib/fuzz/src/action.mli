(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: high-level actions *)

open Base
open Import

(** {1 Types} *)

(** An action as a first-class module. *)
type t = (module Action_types.S)

(** {2 Actions with default weights}

    This module concerns actions bundled with their default weight. This is
    the way in which actions are stored natively in the action table; we
    don't keep default weights in the actual action modules to avoid them
    being scattered over various files. *)
module With_default_weight : sig
  (** Opaque type of actions-with-default-weight. *)
  type t

  val make : action:(module Action_types.S) -> default_weight:int -> t
  (** [make ~action ~default_weight] constructs an action-with-default-weight
      from its action module [action] and default weight [default_weight]. *)

  val ( @-> ) : (module Action_types.S) -> int -> t
  (** [action @-> default_weight] is an infix operator version of [make]. *)

  (** {3 Accessors} *)

  val action : t -> (module Action_types.S)
  (** [action a] gets [a]'s action module. *)

  val default_weight : t -> int
  (** [default_weight a] gets [a]'s default weight. *)

  val name : t -> Common.Id.t
  (** [name a] is shorthand for [A.name], where [A] is [action a]. *)
end

(** {1 Summaries of actions}

    To build these summaries, see {!Pool.summarise} below. *)

(** A summary of the weight assigned to an action in an action pool. *)
module Adjusted_weight : sig
  (** Type of adjusted weight summaries. *)
  type t =
    | Not_adjusted of int  (** The user hasn't overridden this weight. *)
    | Adjusted of {original: int; actual: int}
        (** The weight has changed from [original] to [actual]. *)

  (** Adjusted weights may be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** A summary of a fuzzer action after weight adjustment. *)
module Summary : sig
  (** Opaque type of summaries. *)
  type t

  val make : weight:Adjusted_weight.t -> readme:string -> t
  (** [make ~weight ~summary] makes a fuzzer action summary. *)

  val of_action : ?user_weight:int -> With_default_weight.t -> t
  (** [of_action ?user_weight ~action] makes a fuzzer action summary using an
      action [action] and the given user weighting [user_weight]. *)

  val weight : t -> Adjusted_weight.t
  (** [weight summary] gets the final 'adjusted' weight of the action
      described by [summary]. *)

  val readme : t -> string
  (** [readme summary] gets the README of the action described by [summary]. *)

  (** Summaries may be pretty-printed. *)
  include Pretty_printer.S with type t := t

  val pp_map : t Map.M(Common.Id).t Fmt.t
  (** [pp_map f map] pretty-prints a map of summaries [map] on formatter [f]. *)

  val pp_map_terse : t Map.M(Common.Id).t Fmt.t
  (** [pp_map_terse f map] is like {!pp_map}, but only prints names and
      weights. *)
end

(** A weighted pool of fuzzer actions. *)
module Pool : sig
  (** Action lists are just weighted lists of first-class action modules. *)
  type t

  val of_weighted_actions :
    (With_default_weight.t, int option) List.Assoc.t -> t Or_error.t
  (** [of_weighted_actions actions] tries to make a weighted action pool from
      the action-to-weight association given in [actions]. *)

  val summarise : t -> Summary.t Map.M(Common.Id).t
  (** [summarise pool] generates a mapping from action names to summaries of
      each action, including its adjusted weight in the pool. *)

  val pick :
       t
    -> random:Splittable_random.State.t
    -> (t * (module Action_types.S)) Or_error.t
  (** [pick pool ~random] picks a random action module from [pool], returning
      both it and the pool with that action removed. *)
end

(** {1 Helpers for building actions} *)

(** Makes a basic logging function for an action. *)
module Make_log (B : sig
  val name : Common.Id.t
end) : sig
  val log : Common.Output.t -> ('a, Formatter.t, unit) format -> 'a
end

(** Makes a surrounding action. *)
module Make_surround (Basic : sig
  val name : Act_common.Id.t
  (** [name] is the full name of the surround action. *)

  val surround_with : string
  (** [surround_with] is a phrase that specifies with what construct we're
      surrounding the statements. *)

  val readme_suffix : string
  (** [readme_suffix] gives a suffix to append onto the readme; this can be
      the empty string. *)

  val available : Availability.t
  (** [available] is the availability check for this action. It is required
      because, while many surround actions need only the existence of a
      thread, some have more complex needs. *)

  val path_filter : State.t -> Path_filter.t
  (** [path_filter ctx] gets the path filter for this action, modulo the
      availability context [ctx]. *)

  val checkable_path_filter : Path_filter.t
  (** [checkable_path_filter] gets the subset of {!path_filter} that can be
      checked without an availability context. This is a stopgap until the
      fuzzer state monad is able to check the full filter. *)

  module Payload : sig
    type t [@@deriving sexp]

    val gen : Path.Flagged.t -> t Payload_gen.t
    (** [gen path] should generate an inner payload using [path]. *)

    val src_exprs : t -> Act_fir.Expression.t list
    (** [src_exprs pld] gets the list of source expressions, if any, from
        [pld]. These are flagged as having dependencies. *)
  end

  val run_pre :
    Subject.Test.t -> payload:Payload.t -> Subject.Test.t State.Monad.t
  (** [run_pre test ~payload] performs any actions required before wrapping;
      for instance, declaring new variables. *)

  val wrap :
    Subject.Statement.t list -> payload:Payload.t -> Subject.Statement.t
  (** [wrap stms ~payload] surrounds [stms] in the construct induced by
      [payload]. *)
end) :
  Action_types.S with type Payload.t = Basic.Payload.t Payload_impl.Pathed.t
