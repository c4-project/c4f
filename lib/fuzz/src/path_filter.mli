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
    an if-statement?' or 'is the statement inside a dead code block?'. This
    module provides an abstract data type for collecting and applying these
    predicates. *)

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

(** {1 Consuming filters} *)

val is_final_statement_ok : t -> stm:Subject.Statement.t -> bool
(** [is_final_statement_ok filter ~stm] should be applied before constructing
    a [This_stm] reference to [stm], and checks whether such a final
    statement destination is ok according to the predicates in [filter]. *)
