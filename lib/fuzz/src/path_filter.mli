(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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

(** Opaque type of path filters. *)
type t

val empty : t
(** [empty] is the empty path filter, which has no filtering predicates. *)

(** {1 Predicates}

    These consume a path filter and return a path filter with the given
    predicate switched on. *)

val in_dead_code_only : t -> t
(** [in_dead_code_only filter] adds to [filter] the restriction that any path
    must travel through at least one dead-code block. *)

val in_loop_only : t -> t
(** [in_loop_only filter] adds to [filter] the restriction that any path must
    travel through at least one loop. *)

val in_threads_only : t -> threads:Set.M(Int).t -> t
(** [in_threads_only filter ~threads] adds to [filter] the restriction that
    any path must travel through at least one of the threads in [threads].
    Such restrictions are cumulative. *)

module End_check : sig
  (** Type of end checks. *)
  type t =
    | Is_of_class of Act_c_mini.Statement_class.t
        (** Requires the path to reach a statement of the given class. *)
    | Has_no_labels
        (** Requires the path to reach a statement containing no labels. *)
end

val require_end_check : t -> check:End_check.t -> t
(** [require_end_check filter ~check] adds the check expression [check] to
    the set of things to be checked on statements reached by this path. *)

(** {1 Callbacks for the path producers} *)

val update_with_loop : t -> t
(** [update_with_loop filter] updates the internal filter compliance records
    in [filter] with the fact that the path just passed into a loop. *)

val update_with_block_metadata : t -> Metadata.t -> t
(** [update_with_block_metadata filter block_metadata] updates the internal
    filter compliance records in [filter] with the block metadata
    [block_metadata]. This propagates information such as whether the block
    is dead-code. *)

(** {1 Consuming filters} *)

(** {2 Checking paths} *)

val is_thread_ok : t -> thread:int -> bool
(** [is_thread_ok filter ~thread] checks whether paths may descend through
    thread [thread]. This check is done early in the path production process,
    to avoid spurious generation of large amounts of dead paths. *)

val check : t -> unit Or_error.t
(** [check filter ~stm] should be applied before constructing a path that
    doesn't have any more specific filter consume function, and checks
    whether the path has been constructed correctly according to the
    predicates in [filter]. *)

val check_final_statement : t -> stm:Subject.Statement.t -> unit Or_error.t
(** [check_final_statement filter ~stm] should be applied before constructing
    a [This_stm] reference to [stm], and checks whether such a final
    statement destination is ok according to the predicates in [filter]. It
    subsumes [check]. *)

val are_final_statements_ok :
  t -> all_stms:Subject.Statement.t list -> pos:int -> len:int -> bool
(** [are_final_statements_ok filter ~all_stms ~pos ~len] should be used to
    filter all statement-list paths with position [pos] and length [len] with
    respect to statements [all_stms]. It checks whether every statement in
    the given subrange meets the statement predicates in [filter] (as per
    {!check_final_statement}).) *)

(** {2 Checking to see if paths are constructible} *)

val is_constructible : t -> subject:Subject.Test.t -> bool
(** [is_constructible filter ~subject] checks whether it is possible, in
    theory, to construct at least one path over [subject] that satisfies
    [filter]. It returns [true] if so and [false] otherwise. *)
