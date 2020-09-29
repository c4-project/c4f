(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Stock payloads.

    See {!Payload_types.S} for the shape of an payload. *)

open Base
open Import

(** Dummy payload module for actions that take no payload. *)
module None : Payload_types.S with type t = unit

(** Adapts payload generators that don't depend on a generation context. *)
module Pure (Basic : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Base_quickcheck.Generator.t
end) : Payload_types.S with type t = Basic.t

(** {1 Pathed payloads}

    Most payloads require a path to provide context as to whereabouts in the
    test the payload should take affect. This module provides common support
    for payloads with paths. *)
module Pathed : sig
  (** Type of pathed payloads. *)
  type 'a t = {payload: 'a; where: Path.Flagged.t}
  [@@deriving sexp, accessors]

  val make : 'a -> where:Path.Flagged.t -> 'a t
  (** [make payload ~where] makes a payload that expresses the desire to
      apply [payload] at path [where]. *)

  val gen :
       Path_kind.t
    -> (State.t -> Path_filter.t)
    -> (Path.Flagged.t -> 'a Payload_gen.t)
    -> 'a t Payload_gen.t
  (** [gen kind gen_filter gen_payload] lifts the generator [gen_payload] to
      a pathed generator, using [kind] and [gen_filter] to decide which sort
      of path to generate. *)

  (** {2 Boilerplate for applying pathed payloads} *)

  val surround :
       ?filter:Path_filter.t
    -> 'a t
    -> test:Subject.Test.t
    -> f:('a -> Subject.Statement.t list -> Subject.Statement.t)
    -> Subject.Test.t State.Monad.t
  (** [surround ?filter payload ~test ~f] effects a payload [payload] that
      describes a surround action: it takes a list of statements, applies [f]
      to both it and the payload, and produces a 'wrapped' statement that
      somehow 'surrounds' the original statement with no chance of failure. *)

  val insert :
       ?filter:Path_filter.t
    -> 'a t
    -> test:Subject.Test.t
    -> f:('a -> Subject.Statement.t)
    -> Subject.Test.t State.Monad.t
  (** [insert ?filter payload ~test ~f] effects a payload [payload] that
      describes an insertion action: it generates a statement from applying
      [f] to the inner payload with no chance of failure. *)
end

(** Specialised form of {!Pathed} for actions that surround or insert using
    flows based on conditional expressions (whose environments are those of
    all variables in local scope). *)
module Cond_pathed : sig
  (** Conditional-pathed payloads are just pathed expressions. *)
  type t = Fir.Expression.t Pathed.t

  val surround :
       ?filter:Path_filter.t
    -> t
    -> test:Subject.Test.t
    -> f:
         (   Fir.Expression.t
          -> Subject.Statement.t list
          -> Subject.Statement.t)
    -> Subject.Test.t State.Monad.t
  (** [surround ?filter payload ~test ~f] behaves like {!Pathed.surround},
      but also automatically adds dependency edges for the condition
      expression. *)

  val insert :
       ?filter:Path_filter.t
    -> t
    -> test:Subject.Test.t
    -> f:(Fir.Expression.t -> Subject.Statement.t)
    -> Subject.Test.t State.Monad.t
  (** [insert ?filter payload ~test ~f] behaves like {!Pathed.insert}, but
      also automatically adds dependency edges for the condition expression. *)

  val lift_cond_gen :
       (Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t)
    -> (Path.Flagged.t -> Fir.Expression.t Payload_gen.t) Staged.t
  (** [gen_cond cg path] lifts a condition generator [cg] to one that, given
      path [path], generates all condition expressions over the environment
      of all variables in scope at [path]. *)

  (** [Make_surround] builds a fully-functional payload module with the
      conditional-surround payload and a generator that uses the given
      conditional generator. *)
  module Make (Basic : sig
    val kind : Path_kind.t
    (** [kind] is the intended kind of path. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given an environment [env] capturing the
        variables in scope at the point where the if statement is appearing,
        return a Quickcheck generator generating expressions over those
        variables. *)

    val path_filter : State.t -> Path_filter.t
    (** [path_filter ctx] should generate the path filter needed for the
        action, given the availability context [ctx]. *)
  end) : Payload_types.S with type t = t
end
