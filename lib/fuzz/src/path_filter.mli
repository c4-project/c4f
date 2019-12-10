(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

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

type t
(** Opaque type of path filters. *)

val empty : t
(** [empty] is the empty path filter, which has no filtering predicates. *)

(** {1 Predicates}

    These consume a path filter and return a path filter with the given
    predicate switched on. *)

val final_if_statements_only : t -> t
(** [final_if_statements_only filter] adds to [filter] the restriction that
    any statement that is the final destination of the generated path must be
    an if statement. *)

val in_dead_code_only : t -> t
(** [in_dead_code_only filter] adds to [filter] the restriction that any path
    must travel through at least one dead-code block. *)

val in_loop_only : t -> t
(** [in_loop_only filter] adds to [filter] the restriction that any path must
    travel through at least one loop. *)

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

val check : t -> unit Or_error.t
(** [check filter ~stm] should be applied before constructing a path that
    doesn't have any more specific filter consume function, and checks
    whether the path has been constructed correctly according to the
    predicates in [filter]. *)

val check_final_statement : t -> stm:Subject.Statement.t -> unit Or_error.t
(** [is_final_statement_ok filter ~stm] should be applied before constructing
    a [This_stm] reference to [stm], and checks whether such a final
    statement destination is ok according to the predicates in [filter]. It
    subsumes [check]. *)
