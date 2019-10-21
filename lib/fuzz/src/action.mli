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

(** {2 Summaries of actions}

    To build these summaries, see {{!Pool.summarise} Pool.summarise} below. *)

(** A summary of the weight assigned to an action in an action pool. *)
module Adjusted_weight : sig
  (** Type of adjusted weight summaries. *)
  type t =
    | Not_adjusted of int  (** The user hasn't overridden this weight. *)
    | Adjusted of {original: int; actual: int}
        (** The weight has changed from [original] to [actual]. *)

  include
    Pretty_printer.S with type t := t
  (** Adjusted weights may be pretty-printed. *)
end

(** A summary of a fuzzer action after weight adjustment. *)
module Summary : sig
  type t
  (** Opaque type of summaries. *)

  val make : weight:Adjusted_weight.t -> readme:string -> t
  (** [make ~weight ~summary] makes a fuzzer action summary. *)

  val of_action : (module Action_types.S) -> user_weight:int -> t
  (** [of_action action ~user_weight] makes a fuzzer action summary using an
      action module [action] and the given user weighting [user_weight]. *)

  val weight : t -> Adjusted_weight.t
  (** [weight summary] gets the final 'adjusted' weight of the action
      described by [summary]. *)

  val readme : t -> string
  (** [readme summary] gets the README of the action described by [summary]. *)

  include
    Pretty_printer.S with type t := t
  (** Summaries may be pretty-printed. *)
end

(** A weighted pool of fuzzer actions. *)
module Pool : sig
  type t
  (** Action lists are just weighted lists of first-class action modules. *)

  val of_weighted_actions :
    ((module Action_types.S), int) List.Assoc.t -> t Or_error.t
  (** [of_weighted_actions actions] tries to make a weighted action pool
      from the action-to-weight association given in [actions]. *)

  val summarise : t -> Summary.t Id.Map.t
  (** [summarise pool] generates a mapping from action names to summaries of
      each action, including its adjusted weight in the pool. *)

  val pick :
       t
    -> Subject.Test.t
    -> Splittable_random.State.t
    -> (module Action_types.S) State.Monad.t
  (** [pick pool subject rng] is a stateful action that picks a
      weighted-random action module from [pool] that is available on
      [subject], using [pool] as a random number generator. *)
end

(** {2 Helpers for building actions} *)

val always : Subject.Test.t -> bool State.Monad.t
(** [always test] always returns [true] without modifying or inspecting the
    fuzzer state. *)

(** {3 Helpers for building action payloads} *)

module Pure_payload (S : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Base_quickcheck.Generator.t
end) : Action_types.S_payload with type t = S.t
(** Adapts payload generators that don't depend on the state of the program. *)

module No_payload : Action_types.S_payload with type t = unit
(** Dummy payload module for actions that take no payload. *)

(** Makes a basic logging function for an action. *)
module Make_log (B : sig
  val name : Act_common.Id.t
end) : sig
  val log : Act_common.Output.t -> string -> unit
end
