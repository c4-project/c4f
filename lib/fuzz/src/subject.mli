(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: subjects of fuzzing

    This module contains types for programs and litmus tests that are looser
    and lighter than their {{!Act_c_mini} Act_c_mini} versions, and more
    suited to mutation.

    Many of these types take a metadata type parameter, but this is mostly
    just to support creating path types over them; in practice, the only
    metadata type used in the fuzzer is {!Metadata.t}. *)

open Base

(** An item, annotated with information about its source. *)
module With_source : sig
  type 'a t

  val item : 'a t -> 'a
  (** [item x] gets [x]'s underlying item. *)

  val source : 'a t -> [`Existing | `Generated]
  (** [source x] gets [x]'s source. *)

  val make : item:'a -> source:[`Existing | `Generated] -> 'a t
  (** [make ~item ~source] makes a source-annotated item with contents
      [item] and source [source]. *)
end

(** Fuzzable representation of a program. *)
module Program : sig
  type 'meta t =
    { decls: Act_c_mini.Initialiser.t Act_c_mini.Named.Alist.t
    ; stms: 'meta Act_c_mini.Statement.t With_source.t list }
  [@@deriving sexp]
  (** Transparent type of fuzzable programs. *)

  module Path : Path_types.S_function with type 'meta target := 'meta t
  (** Allows production and consumption of random paths over fuzzable
      programs in the same way as normal mini functions. *)

  (** {3 Constructors} *)

  val empty : 'meta t
  (** [empty] is the empty program. *)

  val of_function : unit Act_c_mini.Function.t -> Metadata.t t
  (** [of_litmus func] converts a mini-model C function [func] to the
      intermediate form used for fuzzing. *)

  val has_statements : 'meta t -> bool
  (** [has_statements prog] is true if, and only if, [prog] contains at
      least one statement. *)

  val to_function :
       'meta t
    -> vars:Var.Map.t
    -> id:int
    -> unit Act_c_mini.Function.t Act_c_mini.Named.t
  (** [to_function prog ~vars ~id] lifts a subject-program [prog] with ID
      [prog_id] back into a Litmus function, adding a parameter list
      generated from [vars] and erasing any metadata. *)

  val list_to_litmus :
    'meta t list -> vars:Var.Map.t -> Act_c_mini.Litmus.Lang.Program.t list
  (** [list_to_litmus progs ~vars] lifts a list [progs] of subject-programs
      back into Litmus programs, adding parameter lists generated from
      [vars], and using the physical position of each program in the list to
      generate its thread ID. *)
end

(** Fuzzable representation of a litmus test. *)
module Test : sig
  type 'meta t =
    (Act_c_mini.Constant.t, 'meta Program.t) Act_litmus.Test.Raw.t
  [@@deriving sexp]
  (** Transparent type of fuzzable litmus tests. *)

  val add_new_program : 'meta t -> 'meta t
  (** [add_new_program test] appends a new, empty program onto [test]'s
      programs list, returning the resulting test. *)

  module Path : Path_types.S_program with type 'meta target := 'meta t
  (** Allows production and consumption of random paths over fuzzable tests
      in the same way as normal mini programs. *)

  val of_litmus : Act_c_mini.Litmus.Test.t -> Metadata.t t
  (** [of_litmus test] converts a validated C litmus test [test] to the
      intermediate form used for fuzzing. *)

  val to_litmus :
    'meta t -> vars:Var.Map.t -> Act_c_mini.Litmus.Test.t Or_error.t
  (** [to_litmus subject ~vars] tries to reconstitute a validated C litmus
      test from the subject [subject], using the variable map [vars] to
      reconstitute parameters. It may fail if the resulting litmus is
      invalid---generally, this signifies an internal error. *)

  (** {3 Helpers for mutating tests} *)

  val add_var_to_init :
       'meta t
    -> Act_common.C_id.t
    -> Act_c_mini.Constant.t
    -> 'meta t Or_error.t
  (** [add_var_to_init subject var initial_value] adds [var] to [subject]'s
      init block with the initial value [initial_value]. *)
end
