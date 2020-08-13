(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: variable records and maps

    This module defines the types that the fuzzer uses to store information
    about variables. *)

open Base

(** Variable records *)
module Record : sig
  type t [@@deriving equal]

  (** {3 Constructors} *)

  val make_existing : Act_common.Scope.t -> Act_fir.Type.t -> t
  (** [make_existing scope ty] makes a variable record for a
      non-fuzzer-generated variable of type [ty] and scope [scope]. *)

  val make_generated :
       ?initial_value:Act_fir.Constant.t
    -> Act_common.Scope.t
    -> Act_fir.Type.t
    -> t
  (** [make_generated_global ?initial_value scope ty] makes a variable record
      for a fuzzer-generated variable of scope [scope] and type [ty], with
      initial value [value]. *)

  (** {3 Predicates} *)

  val is_global : t -> bool
  (** [is_global vr] returns whether [vr] is a global variable. *)

  val has_basic_type : t -> basic:Act_fir.Type.Basic.t -> bool
  (** [has_type vr t] returns whether [vr] is known to have the basic type
      [t]. *)

  val is_atomic : t -> bool
  (** [is_atomic vr] returns whether [vr] is an atomic variable. *)

  val was_generated : t -> bool
  (** [was_generated vr] returns whether [vr] was generated by the fuzzer. *)

  val has_dependencies : t -> bool
  (** [has_dependencies vr] returns true if [vr] has a known value that also
      has dependencies. *)

  val has_writes : t -> bool
  (** [has_writes vr] returns true if [vr] is known to have been written to. *)

  val has_known_value : t -> bool
  (** [has_known_value vr] returns whether [vr] has a known value. *)

  (** {3 Properties} *)

  val ty : t -> Act_fir.Type.t
  (** [ty vr] gets the type of [vr], if known. *)

  val try_get_known_value : t -> Act_fir.Constant.t Or_error.t
  (** [try_get_known_value vr] gets the known value of [vr], if one exists.
      If [vr] doesn't have one, an error occurs. *)

  (** {3 Actions} *)

  val map_type : t -> f:(Act_fir.Type.t -> Act_fir.Type.t) -> t
  (** [map_type record ~f] maps [f] over the type of [record]. *)

  val add_dependency : t -> t
  (** [add_dependency record] adds a dependency flag to the known-value field
      of [record].

      This should be done after involving [record] in any atomic actions that
      depend on its known value. *)

  val add_write : t -> t
  (** [add_write record] adds a write flag to the known-value field of
      [record].

      This should be done after involving [record] in any atomic actions that
      write to it, even if they don't change its known value. *)

  val erase_value : t -> t
  (** [erase_value record] erases the known-value field of [record].

      This should be done after involving [record] in any atomic actions that
      modify it. *)
end

(** {2 Variable maps} *)
module Map : sig
  (** Fuzzer variable maps are a specific kind of scoped map. *)
  type t = Record.t Act_common.Scoped_map.t

  (** {3 Constructors} *)

  val make_existing_var_map : Act_fir.Litmus.Test.t -> t Or_error.t
  (** [make_existing_var_map test] tries to generate a var-record map for the
      Litmus-style functions in [test], where each name is registered as an
      existing variable. *)

  (** {3 Queries} *)

  val env_satisfying_all :
       t
    -> scope:Act_common.Scope.t
    -> predicates:(Record.t -> bool) list
    -> Act_fir.Env.t
  (** [env_satisfying_all map ~scope ~predicates] returns a variable
      environment for all variables in [map] in scope with regards to
      [scope], with known types, and for which all predicates in [predicates]
      are true. *)

  val satisfying_all :
       t
    -> scope:Act_common.Scope.t
    -> predicates:(Record.t -> bool) list
    -> Act_common.C_id.t list
  (** [satisfying_all ?tid map ~predicates] returns a list of all variables
      in [map] in scope with regards to [scope] and for which all predicates
      in [predicates] are true. *)

  val exists_satisfying_all :
       t
    -> scope:Act_common.Scope.t
    -> predicates:(Record.t -> bool) list
    -> bool
  (** [exists_satisfying_all map ~scope ~predicates] returns whether there
      exists at least one variable in [map] in scope with regards to [scope]
      and for which all predicates in [predicates] are true. *)

  val scopes_with_vars : t
    -> predicates:(Record.t -> bool) list
    -> Set.M(Act_common.Scope).t
  (** [scopes_with_vars map ~predicates] gets a set of all scopes that
      contain variables satisfying [predicates]. *)

  (** {3 Actions} *)

  val register_var :
       ?initial_value:Act_fir.Constant.t
    -> t
    -> Act_common.Litmus_id.t
    -> Act_fir.Type.t
    -> t
  (** [register_global ?initial_value map var ty] registers a generated
      variable with scoped name [var], type [ty], and optional known initial
      value [initial_value] in [map], returning the resulting new map. *)

  val gen_fresh_var : t -> Act_common.C_id.t Base_quickcheck.Generator.t
  (** [gen_fresh_var map] generates random C identifiers that don't shadow
      existing variables (regardless of thread ID) in [map]. *)

  val add_dependency : t -> id:Act_common.Litmus_id.t -> t
  (** [add_dependency map ~id] adds a dependency flag in [map] for [id],
      returning the resulting new map.

      This should be done after involving [id] in any atomic actions that
      depend on its known value. *)

  val add_write : t -> id:Act_common.Litmus_id.t -> t
  (** [add_write map ~id] adds a write flag in [map] for [id], returning the
      resulting new map.

      This should be done after involving [id] in any atomic actions that
      write to it, even if they don't modify its value. *)

  val erase_value : t -> id:Act_common.Litmus_id.t -> t Or_error.t
  (** [erase_value map ~id] erases the known-value field for any mapping for
      [id] in [map], returning the resulting new map.

      [erase_value] fails if [id] is mapped to a record whose known value
      field is present and has dependencies. This is a precaution to flag up
      unsafe attempts to alter [id]'s value.

      This should be done after involving [id] in any atomic actions that
      modify it. *)
end
