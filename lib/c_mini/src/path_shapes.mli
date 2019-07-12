(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Basic shapes of paths through mini-C programs. *)

open Base

(** A path focusing on a statement. *)
type stm = In_if of ifs | This_stm [@@deriving sexp]

(** A path focusing on a list of statements. *)
and stm_list = Insert of {index: int} | In_stm of {index: int; rest: stm}
[@@deriving sexp]

(** A path focusing on an if-statement. *)
and ifs = In_block of {branch: bool; rest: stm_list} | This_cond
[@@deriving sexp]

(** A path focusing on a function. *)
type func = In_stms of stm_list [@@deriving sexp]

(** A path focusing on a program. *)
type program = In_func of {index: int; rest: func} [@@deriving sexp]

(** {2 Constructors} *)

(** {3 Statement paths} *)

val in_if : ifs -> stm

val this_stm : stm

(** {3 Statement list paths} *)

val insert : int -> stm_list

val in_stm : int -> stm -> stm_list

(** {3 If-statement paths} *)

val in_block : bool -> stm_list -> ifs

val this_cond : ifs

(** {3 Function paths} *)

val in_stms : stm_list -> func

val in_func : int -> func -> program
(** {3 Program paths} *)
