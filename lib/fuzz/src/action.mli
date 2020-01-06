(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: high-level actions *)

open Base
open Act_common

(** {1 Types} *)

type t = (module Action_types.S)
(** An action as a first-class module. *)

(** {2 Actions with default weights}

    This module concerns actions bundled with their default weight. This is
    the way in which actions are stored natively in the action table; we
    don't keep default weights in the actual action modules to avoid them
    being scattered over various files. *)
module With_default_weight : sig
  type t
  (** Opaque type of actions-with-default-weight. *)

  val make : action:(module Action_types.S) -> default_weight:int -> t
  (** [make ~action ~default_weight] constructs an action-with-default-weight
      from its action module [action] and default weight [default_weight]. *)

  (** {3 Accessors} *)

  val action : t -> (module Action_types.S)
  (** [action a] gets [a]'s action module. *)

  val default_weight : t -> int
  (** [default_weight a] gets [a]'s default weight. *)

  val name : t -> Act_common.Id.t
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

  include Pretty_printer.S with type t := t
  (** Adjusted weights may be pretty-printed. *)
end

(** A summary of a fuzzer action after weight adjustment. *)
module Summary : sig
  type t
  (** Opaque type of summaries. *)

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

  include Pretty_printer.S with type t := t
  (** Summaries may be pretty-printed. *)

  val pp_map : t Map.M(Act_common.Id).t Fmt.t
  (** [pp_map f map] pretty-prints a map of summaries [map] on formatter [f]. *)
end

(** A weighted pool of fuzzer actions. *)
module Pool : sig
  type t
  (** Action lists are just weighted lists of first-class action modules. *)

  val of_weighted_actions :
    (With_default_weight.t, int option) List.Assoc.t -> t Or_error.t
  (** [of_weighted_actions actions] tries to make a weighted action pool from
      the action-to-weight association given in [actions]. *)

  val summarise : t -> Summary.t Id.Map.t
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

val always : Subject.Test.t -> param_map:Param_map.t -> bool State.Monad.t
(** [always test ~param_map] always returns [true] without modifying or
    inspecting the fuzzer state. *)

(** Makes a basic logging function for an action. *)
module Make_log (B : sig
  val name : Act_common.Id.t
end) : sig
  val log : Act_common.Output.t -> string -> unit
end
