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
  (** [make ~location ~reason] makes a disable record noting disable location
      [location] and reason [reason]. *)

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
     ?disabled:('spec Act_common.Spec.With_id.t, Disable.t) List.Assoc.t
  -> 'spec Act_common.Spec.Set.t
  -> 'spec t
(** [make ?disabled enabled] builds a listing from the enabled specs
    [enabled] and optional disabled specs [disabled]. *)

(** {2 Accessors} *)

val enabled : 'spec t -> 'spec Act_common.Spec.Set.t
(** [enabled listing] gets every enabled spec in [listing], as a spec table.
    Generally, the IDs of each spec in the table will be fully qualified IDs,
    and the specs themselves will be machine-qualified. *)

val disabled :
  'spec t -> ('spec Act_common.Spec.With_id.t, Disable.t) List.Assoc.t
(** [disabled listing] gets every disabled spec in [listing], alongside the
    reasoning as to why it was disabled. *)

(** {2 Lookup} *)

val get_with_fqid :
     ?id_type:string
  -> ?default_machines:Act_common.Id.t list
  -> 'spec t
  -> fqid:Act_common.Id.t
  -> 'spec Or_error.t

(** {2 Pretty-printing} *)

val pp_qualified_summary : 'spec Fmt.t -> 'spec Qualified.t t Fmt.t
(** [pp_qualified_summary pp_spec] is a helper for implementing a form of
    pretty-printing on lookup listings where:

    - each spec is a single line;
    - each line begins with the machine ID, then the spec ID;
    - fields are space-delimited and terse.

    This sort of summary is (mostly) human-readable, but easily scraped by
    Unix-style scripts.

    The caller should supply their own [box]. *)

val pp_qualified_verbose :
  ?type_str:string -> 'spec Fmt.t -> 'spec Qualified.t t Fmt.t
(** [pp_qualified_verbose ?type_str pp_spec] is a helper for implementing a
    more verbose form of lookup listing pretty-printing.

    [type_str], if given, is used to replace "results" in the headings. *)
