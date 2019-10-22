(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Basic shapes of paths through mini-C programs. *)

open Base

type index = int [@@deriving sexp]
(** An index in a list selector. *)

type length = int [@@deriving sexp]
(** A length in a list selector. *)

type branch = bool [@@deriving sexp]
(** A branch in an if-statement selector. *)

(** A path focusing on a statement. *)
type stm = In_if of ifs | This_stm [@@deriving sexp]

(** A path focusing on a list of statements. *)
and stm_list =
  | Insert of index  (** Inserting one statement at the given index. *)
  | In_stm of index * stm  (** Traversing further into one statement. *)
  | On_stm_range of index * length
      (** Appling something to an entire subrange of statements. *)
[@@deriving sexp]

(** A path focusing on an if-statement. *)
and ifs = In_block of branch * stm_list | This_cond [@@deriving sexp]

(** A path focusing on a function. *)
type func = In_stms of stm_list [@@deriving sexp]

(** A path focusing on a program. *)
type program = In_func of index * func [@@deriving sexp]

(** {2 Constructors} *)

(** {3 Statement paths} *)

val in_if : ifs -> stm

val this_stm : stm

(** {3 Statement list paths} *)

val insert : index -> stm_list

val in_stm : index -> stm -> stm_list

(** {3 If-statement paths} *)

val in_block : branch -> stm_list -> ifs

val this_cond : ifs

(** {3 Function paths} *)

val in_stms : stm_list -> func

val in_func : index -> func -> program
(** {3 Program paths} *)
