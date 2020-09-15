(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

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

(** {2 Require predicates} *)

val req : t -> flags:Set.M(Path_flag).t -> t
(** [req filter ~flags] adds [flags] to those required by [filter]. *)

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

(** {2 Forbid predicates} *)

val not_in_atomic_block : t -> t
(** [not_in_atomic_block filter] adds to [filter] the restriction that any
    path must not travel through an atomic block. *)

val not_in_execute_multi : t -> t
(** [not_in_execute_multi filter] adds to [filter] the restriction that any
    path must not travel through a loop, or any other such construct, that
    can execute multiple times. *)

val transaction_safe : t -> t
(** [transaction_safe] requires the path to reach a statement that is
    'transaction safe'; that is, it can appear inside an 'atomic' block. This
    forbids particular forms of statement and expression. *)

(** {2 End checks} *)

module End_check : sig
  (** Type of end checks. *)
  type t =
    | Stm_class of
        Act_fir.Class_constraint.t * Act_fir.Statement_class.t list
        (** Requires the path to reach a statement satisfying the given
            statement class constraint. *)
    | Has_no_expressions_of_class of Act_fir.Expression_class.t list
        (** Requires the path to reach a statement that has no expressions of
            any of the given classes. *)
end

val require_end_check : t -> check:End_check.t -> t
(** [require_end_check filter ~check] adds the check expression [check] to
    the set of things to be checked on statements reached by this path. *)

(** {1 Consuming filters} *)

(** {2 Checking paths} *)

val is_thread_ok : t -> thread:int -> bool
(** [is_thread_ok filter ~thread] checks whether paths may descend through
    thread [thread]. This check is done early in the path production process,
    to avoid spurious generation of large amounts of dead paths. *)

val check_not : t -> flags:Set.M(Path_flag).t -> unit Or_error.t
(** [check_not filter ~flags] should be applied when adding new flags to a
    path, and checks to make sure the [flags] so far obey the reject
    conditions in [filter]. *)

val check_req : t -> flags:Set.M(Path_flag).t -> unit Or_error.t
(** [check_req filter ~flags] should be applied before constructing any path,
    and checks whether the path's [flags] satisfy the require conditions in
    [filter]. *)

val check_final_statement : t -> stm:Subject.Statement.t -> unit Or_error.t
(** [check_final_statement filter ~stm] should be applied before constructing
    a [This_stm] reference to [stm], and checks whether such a final
    statement destination is ok according to the predicates in [filter]. It
    does not subsume [check]. *)
