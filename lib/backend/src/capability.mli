(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Structures that represent the result of asking a backend whether it can
    do various tasks. *)

open Base

(** {1 Shallow capability introspection} *)

(** Enumeration of flags that are set on a backend instance if it broadly
    supports a given capability. *)
module Flag : sig
  (** Type of capability flags. *)
  type t =
    | Run  (** This backend can run tests directly. *)
    | Make_harness
        (** This backend can produce a harness that can be compiled and run
            to *)
  [@@deriving sexp, compare, equal]

  include Act_utils.Enum_types.S_table with type t := t

  include Comparable.S with type t := t
end

module Summary : sig
  (** Opaque type of capability summaries. *)
  type t

  (** {2 Constructors} *)

  val make : flags:Set.M(Flag).t -> arches:Set.M(Arch).t -> t
  (** [make ~flags ~arches] constructs a summary for a backend that can
      handle the actions described by [flags] and the architectures described
      by [arches]. *)

  (** {2 Accessors} *)

  val flags : t -> Set.M(Flag).t
  (** [flags summary] gets the capability flags of [summary]. *)

  val arches : t -> Set.M(Arch).t
  (** [arches summary] gets the architectures supported by [summary]. *)
end

module Run : sig
  type t =
    | Cannot_run of {why: Error.t}
    | Can_run of {argv_f: input_file:string -> string list Or_error.t}
end

module Make_harness : sig
  type t =
    | Cannot_make_harness of {why: Error.t}
    | Can_make_harness of
        { argv_f:
            input_file:string -> output_dir:string -> string list Or_error.t
        ; run_as: string list }
end
