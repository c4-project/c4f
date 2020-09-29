(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** A weighted pool of fuzzer actions. *)

(** Opaque type of pools. *)
type t

val of_weighted_actions :
     (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t
  -> t Or_error.t
(** [of_weighted_actions actions] tries to make a weighted action pool from
    the action-to-weight association given in [actions]. *)

val summarise : t -> Fuzz.Action.Summary.t Map.M(Common.Id).t
(** [summarise pool] generates a mapping from action names to summaries of
    each action, including its adjusted weight in the pool. *)

val pick :
  t -> random:Splittable_random.State.t -> (t * Fuzz.Action.t) Or_error.t
(** [pick pool ~random] picks a random action from [pool], returning both it
    and the pool with that action removed. *)
