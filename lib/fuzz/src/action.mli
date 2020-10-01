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

(** {1 Helpers for building actions} *)

(** Makes a basic logging function for an action. *)
module Make_log (B : sig
  val name : Common.Id.t
end) : sig
  val log : Common.Output.t -> ('a, Formatter.t, unit) format -> 'a
end

(** Makes a surrounding action. *)
module Make_surround (Basic : Action_types.Basic_surround) :
  Action_types.S with type Payload.t = Basic.Payload.t Payload_impl.Pathed.t

(** An action that does absolutely nothing. *)
module Nop : Action_types.S with type Payload.t = unit
