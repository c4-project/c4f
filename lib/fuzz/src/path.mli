(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Paths through FIR programs.

    - For modules that generate paths over fuzzer subjects, see
      {!Path_producers}.
    - For modules that perform insertion, modification, and other tasks over
      paths, see {!Path_consumers}. *)

open Base

(** {1 Path components} *)

(** An index in a list selector. *)
type index = int [@@deriving sexp, compare, equal]

(** A length in a list selector. *)
type length = int [@@deriving sexp, compare, equal]

(** A branch in an if-statement selector. *)
type branch = bool [@@deriving sexp, compare, equal]

(** {1 Path shapes} *)

(** {2 Base definitions}

    These definitions appear here and not inside their respective submodules
    to break mutually recursive dependencies. Generally, you'll want to use
    the submodules instead. *)

(** A path focusing on a conditionally guarded compound statement (that is,
    an if statement, while loop, or do-while loop). *)
type 'a cond_block = In_block of 'a | This_cond
[@@deriving sexp, compare, equal]

(** A path focusing on a statement. *)
type stm =
  | In_if of (branch * stm_list) cond_block
  | In_loop of stm_list cond_block
  | This_stm
[@@deriving sexp, compare, equal]

(** A path focusing on a list of statements. *)
and stm_list =
  | Insert of index  (** Inserting one statement at the given index. *)
  | In_stm of index * stm  (** Traversing further into one statement. *)
  | On_range of index * length
      (** Appling something to an entire subrange of statements. *)
[@@deriving sexp, compare, equal]

(** {2 Submodules}

    These submodules expose a nicer interface over the recursive types above,
    and also provide paths at the thread and program level. *)

(** A path focusing on an if statement. *)
module If : sig
  type t = (bool * stm_list) cond_block [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val in_branch : branch -> stm_list -> t
  (** [this_cond b rest] focuses on the branch of an if statement given by
      [b], using subpath [rest]. *)

  val this_cond : t
  (** [this_cond] focuses on the conditional of an if statement. *)
end

(** A path focusing on a while or do-while loop statement. *)
module Loop : sig
  type t = stm_list cond_block [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val in_body : stm_list -> t
  (** [in_body rest] focuses on the body of a while loop using subpath
      [rest]. *)

  val this_cond : t
  (** [this_cond] focuses on the conditional of an if statement. *)
end

module Stm : sig
  type t = stm [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val in_if : If.t -> t
  (** [in_if rest] focuses on an if statement using subpath [rest]. *)

  val in_loop : Loop.t -> t
  (** [in_if rest] focuses on a loop using subpath [rest]. *)

  val this_stm : t
  (** [this_stm] focuses on a statement as a whole. *)
end

module Stms : sig
  type t = stm_list [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val insert : index -> t
  (** [insert index] focuses on inserting a new statement at index [index] of
      a statement list. *)

  val in_stm : index -> Stm.t -> t
  (** [in_stm index rest] focuses on an existing statement at index [index]
      of a statement list, using subpath [rest]. *)

  val stm : index -> t
  (** [stm index] is shorthand for [in_stm index Stm.this_stm]. *)

  val on_range : index -> length -> t
  (** [on_range index length] focuses on an [length]-wide slice of a
      statement list starting at [index]. [length] may be 0, in which case
      the path targets the space just before any statement at [index];
      consequently, [on_range (List.length stms) 0] is a valid path. *)
end

(** A path focusing on a single thread. *)
module Thread : sig
  type t = In_stms of stm_list [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val in_stms : stm_list -> t
end

(** A path focusing on a whole program. *)
module Program : sig
  type t = In_thread of index * Thread.t [@@deriving sexp, compare, equal]

  include Pretty_printer.S with type t := t

  (** {3 Constructors} *)

  val in_thread : index -> Thread.t -> t

  (** {3 Accessors} *)

  val tid : t -> int
  (** [tid] gets the thread ID of a program path. *)
end
