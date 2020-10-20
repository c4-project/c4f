(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** {1 Path flags}

    Path flags contain information about the context into which a path
    targets, for instance 'is in atomic block' or 'is in dead code'. These
    flags are produced as a path gets generated, and used to check the path
    against a {!Path_filter}. *)
module Flag : sig
  (** Type of path flags. *)
  type t =
    | Execute_multi_unsafe
        (** The path reaches a statement that is not safe to execute multiple
            times. *)
    | In_atomic  (** The path passes through an atomic block. *)
    | In_dead_code  (** The path passes through a dead-code block. *)
    | In_execute_multi
        (** The path passes through a block that MAY execute multiple times. *)
    | In_loop  (** The path passes through a loop body. *)

  include Utils.Enum_types.Extension_table with type t := t
end

(** {1 Acquiring path flags} *)

val flags_of_metadata : Metadata.t -> Set.M(Flag).t
(** [flags_of_metadata m] gets the path flags that passing through a
    construct with metadata [m] will enable. *)

val flags_of_block : Subject.Block.t -> Set.M(Flag).t
(** [flags_of_block b] gets the path flags that passing through a block [b]
    will enable. *)

val flags_of_flow : Subject.Statement.Flow.t -> Set.M(Flag).t
(** [flags_of_flow f] gets the path flags that passing through a flow block
    [f] will enable; this does not include any flags relating to the flow's
    block (use {!flags_of_block}). *)

val flags_of_stm : Subject.Statement.t -> Set.M(Flag).t
(** [flags_of_stm s] gets the path flags that directly targeting a statement
    [s] will enable; this does not include any flags relating to passing
    through the statement's block (use {!flags_of_flow}, {!flags_of_block},
    etc.). *)

(** {1 Metadata structures} *)

(** Type of metadata. *)
type t = {flags: Set.M(Flag).t} [@@deriving accessors, sexp, compare, equal]

(** We can pretty-print metadata. *)
include Pretty_printer.S with type t := t

val empty : t
(** [empty] is an empty metadata set. *)

val check_contradiction_free : t -> t Or_error.t
(** [check_contradiction_free m] checks to see if there are contradictions in
    [m]. For example, [m] having both 'execute-multi unsafe' and 'in
    execute-multi' is a contradiction. Such contradictions suggest an error
    in an action generator. *)

(** This exists solely to break a nonrec later on. *)
type meta := t

(** {1 Paths with metadata attached}

    Usually, code outside of the path producer and consumer should use
    {!Path.With_meta.t}, which in turn uses this. *)
module With_meta : sig
  (** Type of paths with metadata, predicated on the path type [p]. *)
  type 'p t = {path: 'p; meta: meta}
  [@@deriving accessors, sexp, compare, equal]

  val make : ?meta:meta -> 'p -> 'p t
  (** [make ?meta path] tags a path with optional metadata. *)

  (** We can traverse tagged paths, with [left] being the path and [right]
      the metadata. *)
  include
    Travesty.Bi_traversable_types.S1_left
      with type 'p t := 'p t
       and type right = meta
end
