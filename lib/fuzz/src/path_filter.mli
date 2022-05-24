(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Path filters.

    When generating paths, it can be useful to restrict the possible targets
    of the path based on various predicates such as 'is the final statement
    an if-statement?' or 'is the statement inside a dead code block?'.

    This module provides an abstract data type for collecting and applying
    these predicates, as well as tracking information about the path as it is
    constructed that can be filtered at the end of its construction. *)

open Base
open Import

(** Opaque type of path filters. *)
type t

(** We can combine path filters using [+], and the empty path filter is
    [zero]. *)
include Container.Summable with type t := t

(** {1 Building path filters} *)

val add_if : t -> when_:bool -> add:t -> t
(** [add_if x ~when_ ~add] is [x + add] when [when_] is true, and [x]
    otherwise. *)

(** {2 Filters that constrain path metadata} *)

val require_meta : Path_meta.t -> t
(** [require_meta meta] is a path filter that requires every piece of
    metadata in [meta] to be present. *)

val require_flags : Set.M(Path_meta.Flag).t -> t
(** [require_flags flags] is a path filter that requires every flag in
    [flags] to be present. *)

val require_flag : Path_meta.Flag.t -> t
(** [require_flag flag] is a path filter that requires [flag] to be present. *)

val forbid_flags : Set.M(Path_meta.Flag).t -> t
(** [require_flags flags] is a path filter that forbids any flag in [flags]
    from being present. *)

val forbid_flag : Path_meta.Flag.t -> t
(** [forbid_flag flag] is a path filter that forbids [flag] from being
    present. *)

(** {2 More complex filters} *)

val in_threads_only : Set.M(Int).t -> t
(** [in_threads_only threads] requires that any path must travel through at
    least one of the threads in [threads]. Such restrictions are cumulative. *)

val in_thread_with_variables :
  Var.Map.t -> predicates:(Var.Record.t -> bool) list -> t
(** [in_thread_with_variables vars ~predicates] is a filter that requires the
    path to pass through a thread with access to at least one thread for
    which a variable in [vars], satisfying all of [predicates], is in scope *)

val transaction_safe : t
(** [transaction_safe] requires that any path must reach statements that are
    'transaction safe'; that is, it can appear inside an 'atomic' block. This
    forbids particular forms of statement and expression. *)

val live_loop_surround : t
(** [live_loop_surround] contains the restrictions that should apply on any
    attempt to surround statements with a live-code loop. *)

(** {2 End checks}

    These don't apply to the statements we are adding with insertion paths;
    only the statements reached by transforms and transform-lists. *)
module End_check : sig
  (** Type of end checks. *)
  type t =
    | Stm_class of Fir.Class_constraint.t * Fir.Statement_class.t list
        (** Requires the path to reach a statement satisfying the given
            statement class constraint. *)
    | Stm_no_meta_restriction of Metadata.Restriction.t
        (** Requires the path to not reach a statement whose metadata,
            directly or recursively, contains the given restriction. *)
    | Has_no_expressions_of_class of Fir.Expression_class.t list
        (** Requires the path to reach a statement that has no expressions of
            any of the given classes. *)

  include Comparable.S with type t := t
end

val require_end_checks : Set.M(End_check).t -> t
(** [require_end_checks checks] requires that any statement(s) reached by
    this path meet all the end checks in [checks]. *)

val require_end_check : End_check.t -> t
(** [require_end_check check] requires that any statement(s) reached by this
    path meet the end check [check]. *)

val anchor : Path_meta.Anchor.t -> t
(** [anchor anc] requires that any statement(s) reached by this path, or any
    insertion locations proposed by this path, respect the anchoring [anc]. *)

(** {2 Blocks} *)
module Block : sig
  (** Type of block restrictions. *)
  type t =
    | Top (* Block is at the top of a function/thread. *)
    | If of bool option (* Block is the given branch of an if statement. *)
    | Flow of Fir.Statement_class.Flow.t option

  (* Block is a flow block with the given classification. *)
end

val ends_in_block : Block.t -> t
(** [ends_in_block b] requires that the inmost block of the path matches [b].

    To check that a path passes through, or does not pass through, a
    particular block, use path flags. *)

(** {1 Consuming filters}

    Various parts of a path filter should be checked at various different
    stages of path production and consumption; hence, there is no one 'check'
    function. *)

val check_thread_ok : t -> thread:int -> unit Or_error.t
(** [check_thread_ok filter ~thread] checks whether paths may descend through
    thread [thread]. This check is done early in the path production process,
    to avoid spurious generation of large amounts of dead paths. *)

val check_not : t -> meta:Path_meta.t -> unit Or_error.t
(** [check_not filter ~meta] should be applied when adding new metadata to a
    path, and checks to make sure the [meta] so far obey the reject
    conditions in [filter]. *)

val check_req : t -> meta:Path_meta.t -> unit Or_error.t
(** [check_req filter ~meta] should be applied before constructing any path,
    and checks whether the path's metadata satisfy the require conditions in
    [filter]. *)

val check_final_statement : t -> stm:Subject.Statement.t -> unit Or_error.t
(** [check_final_statement filter ~stm] should be applied before constructing
    a [This_stm] reference to [stm], and checks whether such a final
    statement destination is ok according to the predicates in [filter]. *)

val check_anchor : ?anchor:Path_meta.Anchor.t -> t -> unit Or_error.t
(** [check_anchor ?anchor filter] checks whether the anchor represented by
    [anchor], if any, satisfies the anchor requirement in [filter]. *)

(** [check_anchor filter ~path ~block_len] should be applied before
    constructing any path targeting a member of a block of length
    [block_len], and checks whether [path] is properly anchored within it. *)

val check_block : t -> block:Block.t -> unit Or_error.t
(** [check_block filter ~block] should be applied before constructing any
    path whose inmost block is [block], and checks whether the filter allows
    it. *)
