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
  -> queue_flag:Fuzz.Flag.t
  -> t Or_error.t
(** [of_weighted_actions actions ~queue_flag] tries to make a weighted action
    pool from the action-to-weight association given in [actions], using
    [queue_flag] to determine when to access the recommendation queue. *)

(** {1 Using pools} *)

val recommend : t -> names:Common.Id.t list -> t Or_error.t
(** [recommend pool ~names] enqueues the actions with names [names] into
    [pool]'s recommendation queue. The actions are queued in order, so that
    the first action is chosen first. The actions must have been in the
    initial deck; otherwise, an error occurs. *)

val pick :
  t -> random:Splittable_random.State.t -> (Fuzz.Action.t * t) Or_error.t
(** [pick pool ~random] picks a random action from [pool], returning both it
    and the pool with that action removed. Use [reset] to prepare the
    returned pool for the next [pick], rather than reloading the previous
    pool. *)

val reset : t -> t
(** [reset pool] reinstates into [pool] any actions removed by [pick]. *)
