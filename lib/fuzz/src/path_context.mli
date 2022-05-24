(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Context for path production and consumption

    Path production and (safe) consumption both depend on knowing information
    about the set of flags currently active on the path, the path filter it
    is supposed to satisfy, and the kind of path it is. This module contains
    the shared context type used by both types of path operation. *)

open Base
open Import

(** Type of context, predicated on the kind information [k]. *)
type 'k t

(** {1 Construction and manipulation} *)

val init : ?filter:Path_filter.t -> 'k -> 'k t
(** [init ?filter k] constructs an initial context with kind [k] and optional
    path filter [filter]. *)

val add_flags : 'k t -> Set.M(Path_meta.Flag).t -> 'k t Or_error.t
(** [add_flags ctx flags] registers [flags] in [ctx]. *)

val block_kind : ('i, Path_filter.Block.t, 'k t, [< field]) Accessor.t
(** [block_kind] is an accessor for the inmost block kind in [ctx]. *)

val block_len : ('i, int, 'k t, [< field]) Accessor.t
(** [block_len] is an accessor for the inmost block length in [ctx]. *)

val update_anchor : 'k t -> span:Utils.My_list.Span.t -> 'k t
(** [update_anchor ctx] uses [span] and the last entered block length to
    deduce an anchor, and updates [ctx]'s metadata accordingly. *)

(** {1 Using context data} *)

val kind : 'k t -> 'k
(** [kind ctx] gets the kind of path [ctx] is being used to produce/consume. *)

val lift_path : _ t -> path:'p -> 'p Path_meta.With_meta.t
(** [lift_path ctx ~path] adds flag information to [path] taken from [ctx]. *)

(** {2 Checking the path filter} *)

val check_filter_req : _ t -> unit Or_error.t
(** [check_filter_req ctx] checks [ctx]'s filter's positive metadata and
    block-type requirements against the context state. *)

val check_filter_stm : _ t -> stm:Subject.Statement.t -> unit Or_error.t
(** [check_filter_stm ctx ~stm] performs [ctx]'s statement end-checks on
    [stm]. *)

val check_filter_stms :
  _ t -> stms:Subject.Statement.t list -> unit Or_error.t
(** [check_filter_stms ctx ~stms] performs [ctx]'s statement end-checks on
    every statement in [stms]. *)

val check_thread_ok : _ t -> thread:int -> unit Or_error.t
(** [check_thread_ok ctx] checks that [ctx]'s filter allows entering thread
    [thread]. *)

val check_anchor : _ t -> unit Or_error.t
(** [check_anchor ctx] checks [ctx]'s filter to see whether [span] (usually
    [Path.Stms.span path]) meets the anchoring requirements. *)

val check_end : _ t -> stms:Subject.Statement.t list -> unit Or_error.t
(** [check_end ctx ~stms] runs {!check_filter_req}, {!check_filter_stms}, and
    {!check_anchor} simultaneously. It should be used for consuming paths,
    and producing statement-based paths; statement-list paths *)
