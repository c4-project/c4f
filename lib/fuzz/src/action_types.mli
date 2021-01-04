(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures of actions and their components. *)

open Base
open Import

(** Module type for the bits of metadata that are common across practically
    every action module type. *)
module type Basic_meta = sig
  (** Type of payload, usually erased out in descendent types. *)
  type pld

  val name : Common.Id.t
  (** [name] is the name of the action, as an act identifier. *)

  val recommendations : pld -> Common.Id.t list
  (** [recommendations p] contains the names of any actions that this action
      'recommends' to run immediately after this one, given the prospective
      payload [p]. *)
end

(** {1 Module type of fuzzer actions} *)
module type S = sig
  val readme : string Lazy.t
  (** [readme] evaluates to a long synopsis of this action. *)

  (** {2 Choosing and generating} *)

  (** The type of any payload on which this action depends. *)
  module Payload : Payload_types.S

  val available : Availability.t
  (** [available] is an availability check that must hold before the action
      can be chosen. *)

  (** {2 Metadata} *)
  include Basic_meta with type pld := Payload.t

  (** {2 Running} *)

  val run :
    Subject.Test.t -> payload:Payload.t -> Subject.Test.t State.Monad.t
  (** [run subject ~payload] is a stateful computation that runs this action
      on [subject] with payload [payload]. *)
end

(** {1 Module types of specific kinds of fuzzer action} *)

(** Specialised input signature of surround actions *)
module type Basic_surround = sig
  val surround_with : string
  (** [surround_with] is a phrase that specifies with what construct we're
      surrounding the statements. *)

  val readme_suffix : string
  (** [readme_suffix] gives a suffix to append onto the readme; this can be
      the empty string. *)

  val available : Availability.t
  (** [available] is the availability check for this action. It is required
      because, while many surround actions need only the existence of a
      thread, some have more complex needs. *)

  val path_filter : State.t -> Path_filter.t
  (** [path_filter ctx] gets the path filter for this action, modulo the
      availability context [ctx]. *)

  module Payload : sig
    type t [@@deriving sexp]

    val gen : Path.With_meta.t -> t Payload_gen.t
    (** [gen path] should generate an inner payload using [path]. *)

    val src_exprs : t -> Act_fir.Expression.t list
    (** [src_exprs pld] gets the list of source expressions, if any, from
        [pld]. These are flagged as having dependencies. *)
  end

  include Basic_meta with type pld := Payload.t Payload_impl.Pathed.t

  val run_pre :
    Subject.Test.t -> payload:Payload.t -> Subject.Test.t State.Monad.t
  (** [run_pre test ~payload] performs any actions required before wrapping;
      for instance, declaring new variables. *)

  val wrap :
    Subject.Statement.t list -> payload:Payload.t -> Subject.Statement.t
  (** [wrap stms ~payload] surrounds [stms] in the construct induced by
      [payload]. *)
end
