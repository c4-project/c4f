(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Fuzzer: state monad *)

open Core_kernel
open Utils
open Act_common

(** Opaque type of states. *)
type t

val init :
     ?o:Output.t
  -> globals:Mini.Type.t C_identifier.Map.t
  -> locals:C_identifier.Set.t
  -> unit
  -> t
(** [init ?o ~globals ~locals ()] creates an initial state with the global
    variable map [globals], and local variable set [locals]. If an output
    context [o] is provided, it can be used for logging verbose/debug
    information during the fuzzing process. *)

val vars : t -> Fuzzer_var.Map.t
(** [vars state] gets the state's variable map. *)

val vars_satisfying_all :
  t -> predicates:(Fuzzer_var.Record.t -> bool) list -> C_identifier.t list
(** [vars_satisfying_all state ~predicates] returns the list of all
    variables in [state]'s variable list that satisfy [predicates]. *)

(** The state monad. *)
module Monad : sig
  include
    Travesty.State_transform.S
    with type state := t
     and module Inner := Or_error

  val with_vars_m : (Fuzzer_var.Map.t -> 'a t) -> 'a t
  (** [with_vars_m f] is a stateful action that binds the stateful action
      [f] over the current variable map. *)

  val with_vars : (Fuzzer_var.Map.t -> 'a) -> 'a t
  (** [with_vars f] is a variant of {{!with_vars_m} with_vars_m} which maps
      across [f] rather than binding. *)

  val register_global :
       ?initial_value:Fuzzer_var.Value.t
    -> Mini.Type.t
    -> C_identifier.t
    -> unit t
  (** [register_global ?initial_value ty var] is a stateful action that
      registers a generated variable [var] of type [ty] and optional known
      value [value] into the state, overwriting any existing variable of the
      same name. *)

  val add_dependency : C_identifier.t -> unit t
  (** [add_dependency var] is a stateful action that adds a dependency flag
      to any known-value record for variable [var].

      This should be done after involving [var] in any atomic actions that
      depend on it having a particular known-value. *)

  val add_write : C_identifier.t -> unit t
  (** [add_write var] is a stateful action that adds a write flag to
      variable [var].

      This should be done after involving [var] in any atomic actions that
      write to it, even if they don't modify it. *)

  val erase_var_value : C_identifier.t -> unit t
  (** [erase_var_value var] is a stateful action that erases any known-value
      information for variable [var].

      This should be done after involving [var] in any atomic actions that
      modify it. *)

  val output : unit -> Output.t t
  (** [output ()] is a stateful action that gets the current output context. *)
end
