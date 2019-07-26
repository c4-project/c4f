(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures of actions and their components. *)

open Base
open Act_common

(** Module type of random state modules.

    Each action takes in a random state record that can be:

    - converted to and from S-expressions;
    - generated, in the context of a fuzzer state, subject, and RNG. *)
module type S_random_state = sig
  type t [@@deriving sexp]
  (** The type of any random state on which this action depends. *)

  type subject
  (** The type of test subjects. *)

  val gen : subject -> random:Splittable_random.State.t -> t State.Monad.t
  (** [gen subject ~random] is a stateful computation that, given subject
      [subject], RNG [random], and and the current state, returns a payload. *)
end

module type Generic = sig
  type subject
  (** The type of test subjects. *)

  val name : Id.t
  (** The name of the action, as an act identifier. *)

  val readme : unit -> string
  (** [readme ()] builds a long synopsis of this action. *)

  val default_weight : int
  (** The default weight of the action. *)

  module Random_state : S_random_state with type subject := subject
  (** Random state on which this action depends. *)

  val available : subject -> bool State.Monad.t
  (** [available subject] is a stateful computation that, given subject
      [subject] and the current state, decides whether this action can run
      (given any member of [Random_state.t]). *)

  val run : subject -> Random_state.t -> subject State.Monad.t
  (** [run subject payload] is a stateful computation that runs this action
      on [subject] with payload [payload]. *)
end

module type S = Generic with type subject := Subject.Test.t
