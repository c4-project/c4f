(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: high-level actions *)

open Base
module Ac := Act_common

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

  (** {3 Accessors} *)

  val action : t -> (module Action_types.S)
  (** [action a] gets [a]'s action module. *)

  val default_weight : t -> int
  (** [default_weight a] gets [a]'s default weight. *)

  val name : t -> Ac.Id.t
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

  val pp_map : t Map.M(Ac.Id).t Fmt.t
  (** [pp_map f map] pretty-prints a map of summaries [map] on formatter [f]. *)

  val pp_map_terse : t Map.M(Ac.Id).t Fmt.t
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

  val summarise : t -> Summary.t Map.M(Ac.Id).t
  (** [summarise pool] generates a mapping from action names to summaries of
      each action, including its adjusted weight in the pool. *)

  val pick :
       t
    -> subject:Subject.Test.t
    -> random:Splittable_random.State.t
    -> param_map:Param_map.t
    -> (module Action_types.S) State.Monad.t
  (** [pick pool ~subject ~random ~param_map] is a stateful action that picks
      a weighted-random action module from [pool] that is available on
      [subject], using [random] as a random number generator and [param_map]
      as the map of configured fuzzer parameters. *)
end

(** {1 Helpers for building actions} *)

(** Makes a basic logging function for an action. *)
module Make_log (B : sig
  val name : Ac.Id.t
end) : sig
  val log : Ac.Output.t -> ('a, Formatter.t, unit) format -> 'a
end

(** Makes a surrounding action.

    This action template expects that the surrounding action is sound if it
    surrounds zero statements. *)
module Make_surround (Basic : sig
  val name : Act_common.Id.t
  (** [name] is the full name of the surround action. *)

  val surround_with : string
  (** [surround_with] is a phrase that specifies with what construct we're
      surrounding the statements. *)

  module Payload : sig
    include Action_types.S_payload

    val where : t -> Path.Program.t
    (** [where pld] gets the transformation path from [pld]. *)
  end

  val wrap :
    Subject.Statement.t list -> payload:Payload.t -> Subject.Statement.t
  (** [wrap stms ~payload] surrounds [stms] in the construct induced by
      [payload]. *)
end) : Action_types.S with type Payload.t = Basic.Payload.t
