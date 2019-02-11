(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Fuzzer: state monad *)

open Core_kernel
open Utils

type t
(** Opaque type of states. *)

val init
  :  Splittable_random.State.t
  -> Mini.Type.t C_identifier.Map.t
  -> C_identifier.Set.t
  -> t
(** [init rng globals locals] creates an initial state with the
    random number generator [rng], global variable map [globals],
    and local variable set [locals]. *)

(** The state monad. *)
module Monad : sig
  include Travesty.State_transform.S
    with type state := t
     and module Inner := Or_error
  ;;

  val with_vars_m : (Fuzzer_var.Map.t -> 'a t) -> 'a t
  (** [with_vars_m f] is a stateful action that binds the stateful
     action [f] over the current variable map. *)

  val with_vars : (Fuzzer_var.Map.t -> 'a) -> 'a t
  (** [with_vars f] is a variant of {{!with_vars_m}with_vars_m} which
      maps across [f] rather than binding. *)

  val with_rng_m :  (Splittable_random.State.t -> 'a t) -> 'a t
  (** [with_rng_m f] is a stateful action that binds the stateful
      action [f] over the state monad's random number generator. *)

  val with_rng :  (Splittable_random.State.t -> 'a) -> 'a t
  (** [with_rng f] is a variant of {{!with_rng_m}with_rng_m} which
      maps across [f] rather than binding. *)

  val register_global
     :  ?initial_value:Fuzzer_var.Value.t
     -> Mini.Type.t
     -> C_identifier.t
     -> unit t
  (** [register_global ?initial_value ty var] is a stateful action that
      registers a generated variable [var] of type [ty] and optional
      known value [value] into the state,
      overwriting any existing variable of the same name. *)

   val gen_and_register_fresh_var
     :  ?initial_value:Fuzzer_var.Value.t
     -> Mini.Type.t
     -> C_identifier.t t
  (** [gen_and_register_fresh_var ?value ty] is a stateful action that
      generates a variable name not already
      registered in the state, then registers it as a generated
      variable of type [ty] and optional value [value].  It returns
      the generated variable name. *)

   val erase_var_value : C_identifier.t -> unit t
   (** [erase_var_value var] is a stateful action that erases any
      known-value information for variable [var].

      This should be done after involving [var] in any atomic actions
      that modify it. *)
end
