(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: subjects of fuzzing

    This module contains types for thread and litmus tests that are looser
    and lighter than their {!C4f_fir} versions, and more suited to mutation.

    Many of these types take a metadata type parameter, but this is mostly
    just to support creating path types over them; in practice, the only
    metadata type used in the fuzzer is {!Metadata.t}. *)

open Base
open Import

(** {1 Shorthand for FIR constructs with subject metadata}

    Compared to the metadata-parametric forms in {!C4f_fir}, these don't
    contain any accessors/constructors/traversals, but do group useful
    functionality that depends on knowing the metadata type.

    See also {!Label}. *)

(** {2 Subject statements} *)

module Statement : sig
  type t = Metadata.t Fir.Statement.t [@@deriving sexp]

  module If : sig
    type t = Metadata.t Fir.Statement.If.t [@@deriving sexp]
  end

  module Flow : sig
    type t = Metadata.t Fir.Statement.Flow_block.t [@@deriving sexp]
  end

  include Fir.Statement_types.S_with_meta with type t := t

  (** {3 Constructors} *)

  val make_generated_prim : Fir.Prim_statement.t -> t
  (** [make_generated_prim prim] lifts a primitive statement to a generated
      subject statement. *)
end

(** {2 Subject blocks} *)

module Block : sig
  type t = (Metadata.t, Statement.t) Fir.Block.t

  (** {3 Specialised constructors}

      For normal constructors, see {!C4f_fir.Block}. *)

  val make_existing : ?statements:Statement.t list -> unit -> t
  (** [make_existing ?statements ()] makes a block, optionally containing the
      existing statements [statements], that is metadata-marked as existing
      before fuzzing. *)

  val make_generated : ?statements:Statement.t list -> unit -> t
  (** [make_generated ?statements ()] makes a block, optionally containing
      the existing statements [statements], that is metadata-marked as
      generated by the fuzzer. *)

  val make_dead_code : ?statements:Statement.t list -> unit -> t
  (** [make_generated ?statements ()] makes a block, optionally containing
      the existing statements [statements], that is metadata-marked as
      generated and dead-code-by-construction by the fuzzer. *)
end

(** {1 Fuzzable representation of a thread} *)
module Thread : sig
  (** Transparent type of fuzzable programs. *)
  type t =
    { decls: Fir.Initialiser.t C4f_common.C_named.Alist.t
    ; stms: Statement.t list }
  [@@deriving sexp]

  (** {2 Constructors} *)

  val empty : t
  (** [empty] is the empty program. *)

  val make :
       ?decls:Fir.Initialiser.t C4f_common.C_named.Alist.t
    -> ?stms:Statement.t list
    -> unit
    -> t
  (** [make ?decls ?stms ()] makes a thread with the given decls and
      statements (defaulting to empty). *)

  val of_function : unit Fir.Function.t -> t
  (** [of_litmus func] converts a FIR C function [func] to the intermediate
      form used for fuzzing. *)

  (** {2 Manipulating threads} *)

  val map_decls :
       t
    -> f:
         (   Fir.Initialiser.t C4f_common.C_named.t
          -> Fir.Initialiser.t C4f_common.C_named.t )
    -> t
  (** [map_decls thd ~f] maps [f] over each decl in [thd]. *)

  val to_function :
    t -> vars:Var.Map.t -> id:int -> unit Fir.Function.t C4f_common.C_named.t
  (** [to_function prog ~vars ~id] lifts a subject-program [prog] with ID
      [prog_id] back into a Litmus function, adding a parameter list
      generated from [vars] and erasing any metadata. *)

  val list_to_litmus :
    t list -> vars:Var.Map.t -> Fir.Litmus.Lang.Program.t list
  (** [list_to_litmus progs ~vars] lifts a list [progs] of subject-programs
      back into Litmus programs, adding parameter lists generated from
      [vars], and using the physical position of each program in the list to
      generate its thread ID. *)

  val exists_top_statement : t -> f:(Statement.t -> bool) -> bool
  (** [exists_top_statement thread ~f] is true if there exists a top-level
      statement in [thread] such that [f] holds. *)
end

(** Fuzzable representation of a litmus test. *)
module Test : sig
  (** Transparent type of fuzzable litmus tests. *)
  type t = (Fir.Constant.t, Thread.t) C4f_litmus.Test.Raw.t [@@deriving sexp]

  val add_new_thread : t -> t
  (** [add_new_thread test] appends a new, empty thread onto [test]'s threads
      list, returning the resulting test. *)

  val of_litmus : Fir.Litmus.Test.t -> t
  (** [of_litmus test] converts a validated C litmus test [test] to the
      intermediate form used for fuzzing. *)

  val to_litmus : t -> vars:Var.Map.t -> Fir.Litmus.Test.t Or_error.t
  (** [to_litmus subject ~vars] tries to reconstitute a validated C litmus
      test from the subject [subject], using the variable map [vars] to
      reconstitute parameters. It may fail if the resulting litmus is
      invalid---generally, this signifies an internal error. *)

  (** {3 Availability queries} *)

  val exists_thread : t -> f:(Thread.t -> bool) -> bool
  (** [exists_thread test ~f] is true if there exists a thread in [test] such
      that [f] holds. *)

  val exists_top_statement : t -> f:(Statement.t -> bool) -> bool
  (** [exists_top_statement test ~f] is true if there exists a top-level
      statement in [test] such that [f] holds. *)

  val has_statements : ?matching:Fir.Statement_class.t list -> t -> bool
  (** [has_statements ?matching test] is true if there exists a statement at
      any level in [test] that matches at least one of [matching]; if
      [matching] is empty or not given, we just check to see if any
      statements exist. *)

  val has_statements_not_matching :
    t -> one_of:Fir.Statement_class.t list -> bool
  (** [exist_statement_not_matching test ~templates] is true if there exists
      a statement at any level in [test] that fails to match at least one of
      [one_of]. *)

  (** {3 Helpers for mutating tests} *)

  val declare_var :
    t -> C4f_common.Litmus_id.t -> Fir.Initialiser.t -> t Or_error.t
  (** [declare_var subject var init] adds [var] with initialiser [init] to
      [subject]'s init block or thread initialisers with the initial value
      [initial_value]. *)
end
