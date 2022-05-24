(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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

(** {2 Acquiring path flags} *)

val flags_of_metadata : Metadata.t -> Set.M(Flag).t
(** [flags_of_metadata m] gets the path flags that passing through a
    construct with FIR metadata [m] will enable. *)

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

(** {1 Anchors}

    Path anchors specify the location of a path inside its innermost block,
    and, in filter position, the location that a path must reach. For
    instance, a top-anchored path touches the top of the block; a bottom path
    touches the bottom; a full path spans the entirety of the block.

    Anchors work over insert paths (full anchors imply that we're inserting
    into an empty block), transform paths (full anchors imply that the
    statement we're transforming is the only member of the block), and
    transform-list paths (full anchors imply we're transforming every
    statement in a block). *)
module Anchor : sig
  (** Type of anchors.

      Note that the compare instance here is NOT inclusion, but just the
      total ordering on the definitions below. *)
  type t =
    | Top  (** Path must be located at the top of a block. *)
    | Bottom  (** Path must be located at the bottom of a block. *)
    | Full  (** Path must access the whole block. *)
  [@@deriving sexp, compare, equal, quickcheck]

  val incl_opt : ?includes:t -> t option -> bool
  (** [incl_opt ?includes x] is true if [x] includes [includes]. [Full]
      includes everything, and everything includes [None] (so, anchors form a
      lattice). *)

  val merge_opt : t option -> t option -> t option
  (** [merge_opt l r] merges two optional anchors, according to the lattice
      formed by [incl_opt]. *)

  val of_dimensions : span:Utils.My_list.Span.t -> block_len:int -> t option
  (** [of_dimensions ~span ~block_len] determines the anchor, if any, from
      the given dimensions. *)

  val top : ('i, bool, t option, [< field]) Accessor.t
  (** [top] is a field accessor that allows testing, setting, and clearing
      the top anchor on optional anchors. *)

  val bottom : ('i, bool, t option, [< field]) Accessor.t
  (** [bottom] is a field accessor that allows testing, setting, and clearing
      the top anchor on optional anchors. *)
end

(** {1 Metadata structures} *)

(** Type of metadata. *)
type t = {flags: Set.M(Flag).t; anchor: Anchor.t option}
[@@deriving accessors, sexp, compare, equal]

(** We can pretty-print metadata. *)
include Pretty_printer.S with type t := t

(** Metadata is a monoid: [zero] is an empty metadata set; [+] merges two
    sets of metadata.

    Note [+] is NOT the right operator for adding metadata from a block to
    the existing metadata for a path, as it merges inmost-block-specific
    metadata rather than replacing it. [+] is intended mostly for path filter
    construction. *)
include Container.Summable with type t := t

val add_flags : t -> flags:Set.M(Flag).t -> t
(** [add_flags meta ~flags] is shorthand for unifying [flags] and [meta]'s
    existing flags, producing a new metadata structure. *)

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
