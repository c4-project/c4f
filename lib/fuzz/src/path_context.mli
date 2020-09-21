(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Context for path production and consumption

    Path production and (safe) consumption both depend on knowing information
    about the set of flags currently active on the path, the path filter it
    is supposed to satisfy, and the kind of path it is. This module contains
    the shared context type used by both types of path operation. *)

open Base

(** Type of context, predicated on the kind information [k]. *)
type 'k t

(** {1 Construction and manipulation} *)

val init : ?filter:Path_filter.t -> 'k -> 'k t
(** [init ?filter k] constructs an initial context with kind [k] and optional
    path filter [filter]. *)

val add_flags : 'k t -> Set.M(Path_flag).t -> 'k t Or_error.t
(** [add_flags ctx flags] registers [flags] in [ctx]. *)

(** {1 Using context data} *)

val kind : 'k t -> 'k
(** [kind ctx] gets the kind of path [ctx] is being used to produce/consume. *)

val lift_path : _ t -> path:'p -> 'p Path_flag.Flagged.t
(** [lift_path ctx ~path] adds flag information to [path] taken from [ctx]. *)

(** {2 Checking the path filter} *)

val check_filter_req : _ t -> unit Or_error.t
(** [check_filter_req ctx] checks [ctx]'s filter flags against those it
    holds. *)

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

val check_anchor :
  _ t -> path:Path.Stms.t -> block_len:int -> unit Or_error.t
(** [check_anchor ctx] checks [ctx]'s filter to see whether [path] meets the
    anchoring requirements within a block of length [block_len]. *)
