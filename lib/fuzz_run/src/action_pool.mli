(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** A pool of fuzzer actions.

    The action pool contains both a weighted list of actions, known as the
    'deck', and a queue of 'recommended' actions that have a higher chance of
    being selected over deck actions. *)

(** Opaque type of pools. *)
type t

(** {1 Making pools} *)

val of_weighted_actions :
     (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t
  -> accept_rec_flag:Fuzz.Flag.t
  -> use_rec_flag:Fuzz.Flag.t
  -> t Or_error.t
(** [of_weighted_actions actions ~accept_rec_flag ~use_rec_flag] tries to
    make a weighted action pool from the action-to-weight association given
    in [actions], using [accept_rec_flag] to determine when to add to the
    recommendation queue, and [use_rec_flag] to determine when to access the
    recommendation queue. *)

(** {1 Using pools} *)

val recommend :
  t -> names:Common.Id.t list -> random:Splittable_random.State.t -> t
(** [recommend pool ~names ~random] enqueues the actions with names [names]
    into [pool]'s recommendation queue. The actions are queued in order, so
    that the first action is chosen first. The actions must have been in the
    initial deck to be accepted. Whether the pool accepts the recommendations
    also depends on [random] and the pool's accept flag. *)

val pick :
     t
  -> random:Splittable_random.State.t
  -> (Fuzz.Action.t option * t) Or_error.t
(** [pick pool ~random] picks a random action from [pool]. If at least one
    action is available, [pick] returns both it and the pool with that action
    removed; else, it returns the pool unchanged. Use [reset] to prepare the
    returned pool for the next [pick], rather than reloading the previous
    pool. *)

val pick_many :
     t
  -> max:int
  -> random:Splittable_random.State.t
  -> (Fuzz.Action.t list * t) Or_error.t
(** [pick_many pool ~max ~random] picks at most [max] actions from [pool]. It
    may pick fewer than [max] if it runs out of actions to pick. *)

val reset : t -> t
(** [reset pool] reinstates into [pool] any actions removed by [pick]. *)
