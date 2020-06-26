(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures of actions and their components. *)

open Base
open Act_common

(** Module type of payload modules.

    Each action takes in a payload record that can be:

    - converted to and from S-expressions;
    - generated, in the context of a fuzzer state, subject, and RNG. *)
module type S_payload = sig
  (** The type of the payload. *)
  type t [@@deriving sexp]

  val gen :
       Subject.Test.t
    -> random:Splittable_random.State.t
    -> param_map:Param_map.t
    -> t State.Monad.t
  (** [gen subject ~random ~param_map] is a stateful computation that, given
      subject [subject], RNG [random], fuzzer parameter map [param_map], and
      the current state, returns a payload. *)
end

(** Module type of fuzzer actions. *)
module type S = sig
  val name : Id.t
  (** The name of the action, as an act identifier. *)

  val readme : unit -> string
  (** [readme ()] builds a long synopsis of this action. *)

  (** The type of any payload on which this action depends. *)
  module Payload : S_payload

  val available : Availability.t
  (** [available] is an availability check that must hold before the action
      can be chosen. *)

  val run :
    Subject.Test.t -> payload:Payload.t -> Subject.Test.t State.Monad.t
  (** [run subject ~payload] is a stateful computation that runs this action
      on [subject] with payload [payload]. *)
end
