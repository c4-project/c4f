(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer: state monad *)

open Base
open Import

(** {1 Fuzzer states} *)

(** Type of states. *)
type t =
  { o: Common.Output.t [@default Common.Output.silent ()]
  ; labels: Set.M(Common.Litmus_id).t
        [@default Set.empty (module Common.Litmus_id)]
  ; vars: Var.Map.t
  ; params: Param_map.t }
[@@deriving accessors]

(** {2 Constructing an initial fuzzer state} *)

val make :
     ?o:Common.Output.t
  -> ?labels:Set.M(Common.Litmus_id).t
  -> vars:Var.Map.t
  -> params:Param_map.t
  -> unit
  -> t
(** [make ?o ~labels ~vars ~params ()] creates an initial state with the
    label set [labels], variable map [vars], and parameter map [params]. If
    an output context [o] is provided, it can be used for logging
    verbose/debug information during the fuzzing process. *)

val of_litmus :
     ?o:Common.Output.t
  -> Fir.Litmus.Test.t
  -> params:Param_map.t
  -> t Or_error.t
(** [of_litmus ?o lt ~params] tries to create an initial state by extracting
    information from [lt]'s auxiliary data. If an output context [o] is
    provided, it can be used for logging verbose/debug information during the
    fuzzing process. *)

(** {1 State monad} *)
module Monad : sig
  include
    Travesty.State_transform_types.S
      with type state := t
       and module Inner := Or_error

  (** Shorthand for lifting accessors over the state monad. *)
  module Acc : Accessor.Monad.S with type 'x t := 'x t

  (** {2 Liftings} *)

  val with_labels_m : (Set.M(Common.Litmus_id).t -> 'a t) -> 'a t
  (** [with_labels_m f] is a stateful action that binds the stateful action
      [f] over the current label set. *)

  val with_labels : (Set.M(Common.Litmus_id).t -> 'a) -> 'a t
  (** [with_labels f] is a variant of {!with_labels_m} which maps across [f]
      rather than binding. *)

  val with_vars_m : (Var.Map.t -> 'a t) -> 'a t
  (** [with_vars_m f] is a stateful action that binds the stateful action [f]
      over the current variable map. *)

  val with_vars : (Var.Map.t -> 'a) -> 'a t
  (** [with_vars f] is a variant of {!with_vars_m} which maps across [f]
      rather than binding. *)

  (** {2 Queries} *)

  val resolve : Common.C_id.t -> scope:Common.Scope.t -> Common.Litmus_id.t t
  (** [resolve id ~scope] tries to get the Litmus-style ID corresponding to
      the resolution of [id] in scope [scope]. *)

  val output : unit -> Common.Output.t t
  (** [output ()] is a stateful action that gets the current output context. *)

  val get_flag : Common.Id.t -> Flag.t t
  (** [get_flag id] gets the flag with name [id] from the state's parameter
      map. *)

  (** {2 Commands} *)

  val register_var : Common.Litmus_id.t -> Fir.Initialiser.t -> unit t
  (** [register_var var init] is a stateful action that registers a generated
      variable [var] with initialiser [init] into the state, overwriting any
      existing variable of the same name. *)

  val register_and_declare_var :
       Common.Litmus_id.t
    -> Fir.Initialiser.t
    -> Subject.Test.t
    -> Subject.Test.t t
  (** [register_and_declare_var var init test] is a stateful action that
      registers [var] and [init], then inserts a matching declaration into
      [test]. *)

  val register_label : Common.Litmus_id.t -> unit t
  (** [register_label label] is a stateful action that registers a label
      [label], given as a Litmus ID (pair of label name and thread ID), as
      in-use in the test subject. *)

  val add_write : Common.Litmus_id.t -> unit t
  (** [add_write var] is a stateful action that adds a write flag to variable
      [var].

      This should be done after involving [var] in any atomic actions that
      write to it, even if they don't modify it. *)

  val erase_var_value : Common.Litmus_id.t -> unit t
  (** [erase_var_value var] is a stateful action that erases any known-value
      information for variable [var].

      This should be done after involving [var] in any atomic actions that
      modify it. *)

  (** {3 Adding dependency flags} *)

  val add_dependency : Common.Litmus_id.t -> unit t
  (** [add_dependency var] is a stateful action that adds a dependency flag
      to any known-value record for variable [var].

      This should be done after involving [var] in any atomic actions that
      depend on it having a particular known-value. *)

  val add_expression_dependencies :
    Fir.Expression.t -> scope:Common.Scope.t -> unit t
  (** [add_expression_dependencies expr ~scope] is a stateful action that
      adds dependency flags for any known-variable records for variables
      mentioned in [expr], resolving each variable from the perspective of
      scope [scope]. *)

  val add_multiple_expression_dependencies :
    Fir.Expression.t list -> scope:Common.Scope.t -> unit t
  (** [add_multiple_expression_dependencies expr ~scope] is a stateful action
      that adds dependency flags for any known-variable records for variables
      mentioned in each expression in [exprs], resolving each variable from
      the perspective of scope [scope]. *)

  val add_expression_dependencies_at_path :
    Fir.Expression.t list -> path:Path.Flagged.t -> unit t
  (** [add_expression_dependencies_at_path expr ~path] behaves as
      {!act_multiple_expression_dependencies}, but takes the scope from
      [path], and does not add dependencies if [path] is flagged as being
      inside dead code. *)
end
