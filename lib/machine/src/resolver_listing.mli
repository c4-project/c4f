(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Listings of enabled/disabled specs returned by resolvers. *)

open Base

(** {1 Disable records}

    Information about why a spec is marked as disabled in a listing. *)
module Disable : sig
  (** {2 Locations from which a spec is disabled}

      A spec can either be disabled directly, or indirectly through its
      parent machine being disabled. *)
  module Location : sig
    (** Enumeration of spec disable locations. *)
    type t =
      | Machine  (** This spec was disabled at the machine level. *)
      | Spec  (** This spec was disabled directly. *)
  end

  (** {2 Reasons why a spec is disabled} *)
  module Reason : sig
    (** Enumeration of spec disable reasons. *)
    type t =
      | In_config  (** This spec was disabled in the config file. *)
      | Filtered  (** This spec was disabled by a filtering predicate. *)
      | Failed_test of Error.t  (** This spec failed its self-test. *)
  end

  type t
  (** Opaque type of disable records. *)

  (** {2 Constructors} *)

  val make : location:Location.t -> reason:Reason.t -> t
  (** [make ~location ~reason] makes a disable record noting disable
      location [location] and reason [reason]. *)

  val make_failed : location:Location.t -> error:Error.t -> t
  (** [make_failed ~location ~error] makes a disable record noting disable
      location [location] and a test-fail reason derived from the error
      [error]. *)
end

(** {1 Listings themselves} *)

type 'spec t
(** Opaque type of listings. *)

(** {2 Constructors} *)

val make :
  ?disabled:('spec, Disable.t) List.Assoc.t -> 'spec list -> 'spec t
(** [make ?disabled enabled] builds a listing from the enabled specs
    [enabled] and optional disabled specs [disabled]. *)

(** {2 Accessors} *)

val enabled : 'spec t -> 'spec list
(** [enabled listing] gets every enabled spec in [listing]. *)

val disabled : 'spec t -> ('spec, Disable.t) List.Assoc.t
(** [disabled listing] gets every disabled spec in [listing], alongside the
    reasoning as to why it was disabled. *)
