(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for machine-qualified specifications and their lookup.

    Generally, user code won't need to use this module directly; the salient
    parts are imported/used in {{!Qualified} Qualified}. *)

open Base

(** {2 Types for qualified specs} *)

module type S = sig
  type t [@@deriving equal]
  (** Opaque type of the qualified specification itself. *)

  (** {3 Accessors} *)

  val m_spec : t -> Spec.With_id.t
  (** [m_spec spec] accesses the bundled machine specification inside [spec]. *)
end

(** {2 Types for lookups} *)

(** Signature of modules that implement qualified lookup. *)
module type S_lookup = sig
  type t
  (** The qualified final specification type. *)

  val lookup :
       ?defaults:Act_common.Id.t list
    -> Spec.Set.t
    -> fqid:Act_common.Id.t
    -> t Or_error.t
  (** [lookup ?defaults machines ~fqid] looks up a specification using its
      fully qualified ID [fqid] in [machines].

      The lookup first tries [fqid] itself; if this fails, and [defaults] is
      present and non-empty, then it tries again with the result of prefixing
      each ID in [defaults] to [fqid] in turn. *)

  val all : Spec.Set.t -> t list
  (** [all machines] returns a list of qualified forms of all specifications
      reachable from the machines in [machines]. *)
end
