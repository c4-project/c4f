(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: state monad *)

open Base

type t
(** Opaque type of states. *)

(** {2 Constructing an initial fuzzer state} *)

val make : ?o:Act_common.Output.t -> vars:Var.Map.t -> unit -> t
(** [make ?o ~vars ()] creates an initial state with the variable map [vars].
    If an output context [o] is provided, it can be used for logging
    verbose/debug information during the fuzzing process. *)

val of_litmus :
  ?o:Act_common.Output.t -> Act_c_mini.Litmus.Test.t -> t Or_error.t
(** [of_litmus ?o lt] tries to create an initial state by extracting
    information from [lt]'s auxiliary data. If an output context [o] is
    provided, it can be used for logging verbose/debug information during the
    fuzzing process. *)

(** {2 Directly accessing a fuzzer state} *)

val vars : t -> Var.Map.t
(** [vars state] gets the state's variable map. *)

val vars_satisfying_all :
     t
  -> scope:Act_common.Scope.t
  -> predicates:(Var.Record.t -> bool) list
  -> Act_common.C_id.t list
(** [vars_satisfying_all state ~scope ~predicates] returns the list of all
    variables in [state]'s variable list that are in scope at [scope] and
    satisfy [predicates]. *)

(** The state monad. *)
module Monad : sig
  include
    Travesty.State_transform_types.S
      with type state := t
       and module Inner := Or_error

  (** {2 Liftings} *)

  val with_vars_m : (Var.Map.t -> 'a t) -> 'a t
  (** [with_vars_m f] is a stateful action that binds the stateful action [f]
      over the current variable map. *)

  val with_vars : (Var.Map.t -> 'a) -> 'a t
  (** [with_vars f] is a variant of {{!with_vars_m} with_vars_m} which maps
      across [f] rather than binding. *)

  (** {2 Queries} *)

  val resolve :
    Act_common.C_id.t -> scope:Act_common.Scope.t -> Act_common.Litmus_id.t t
  (** [resolve id ~scope] tries to get the Litmus-style ID corresponding to
      the resolution of [id] in scope [scope]. *)

  val output : unit -> Act_common.Output.t t
  (** [output ()] is a stateful action that gets the current output context. *)

  (** {2 Commands} *)

  val register_global :
       ?initial_value:Act_c_mini.Constant.t
    -> Act_c_mini.Type.t
    -> Act_common.C_id.t
    -> unit t
  (** [register_global ?initial_value ty var] is a stateful action that
      registers a generated variable [var] of type [ty] and optional known
      value [value] into the state, overwriting any existing variable of the
      same name. *)

  val add_dependency : Act_common.Litmus_id.t -> unit t
  (** [add_dependency var] is a stateful action that adds a dependency flag
      to any known-value record for variable [var].

      This should be done after involving [var] in any atomic actions that
      depend on it having a particular known-value. *)

  val add_write : Act_common.Litmus_id.t -> unit t
  (** [add_write var] is a stateful action that adds a write flag to variable
      [var].

      This should be done after involving [var] in any atomic actions that
      write to it, even if they don't modify it. *)

  val erase_var_value : Act_common.Litmus_id.t -> unit t
  (** [erase_var_value var] is a stateful action that erases any known-value
      information for variable [var].

      This should be done after involving [var] in any atomic actions that
      modify it. *)
end
