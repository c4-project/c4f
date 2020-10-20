(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The payload generator monad. *)

open Base
open Import

(** {1 Context}

    A payload generation context is an extension of an
    {!Availability.Context.t}, containing also a random number generator and
    action ID. *)
module Context : sig
  (** Type of payload generation contexts. *)
  type t =
    { action_id: Common.Id.t
    ; actx: Availability.Context.t
    ; random: Splittable_random.State.t }
  [@@deriving accessors, make]

  (** {2 Shorthands for things in the availability context} *)

  val state : ('i, State.t, t, [< field]) Accessor.Simple.t
  (** [state] focuses on the state nested inside the availability context. *)
end

(** {1 The generator monad}

    Think of the generator monad as a sort of reader monad over {!Context.t}. *)

include
  Act_utils.Reader_types.S
    with type 'a Inner.t = 'a Or_error.t
     and type ctx = Context.t

val lift_acc :
  (unit -> 'a -> 'b, unit -> ctx -> 'c, [> getter]) Accessor.t -> 'a t
(** [lift_acc acc] is shorthand for [lift (Accessor.get acc)]. *)

(** {2 Let syntax}

    This will move to Travesty's monad extensions eventually, I imagine. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** Syntax for mapping. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** Syntax for binding. *)

(** {2 Constructing generators}

    One can, of course, use [return] to lift a raw value to a generator. *)

val lift : (Context.t -> 'a) -> 'a t
(** [peek f] projects a value from the context using [f]. *)

val lift_state : (State.t -> 'a) -> 'a t
(** [lift_state f] lifts a function from states to values into the monad. *)

val path_with_flags :
  Path_kind.t -> filter:Path_filter.t -> Path.With_meta.t t
(** [path_with_flags kind ~filter] generates a path of kind [kind] respecting
    filter [filter], and returns it with its flags. *)

val fresh_var :
     ?such_that:(Common.Litmus_id.t -> bool)
  -> Common.Scope.t
  -> Common.Litmus_id.t t
(** [fresh_var ?such_that scope] generates a fresh variable at [scope],
    optionally satisfying the predicate [such_that]. *)

(** {3 Common queries} *)

val vars : Var.Map.t t
(** [vars] gets the current variable map at time of generation. *)

val env_at_path :
  ?predicates:(Var.Record.t -> bool) list -> Path.With_meta.t -> Fir.Env.t t
(** [env_at_path ?predicates path] gets the environment of variables in scope
    at [path], according to this payload generator's state snapshot. If
    [predicates] is present, the environment will contain only those
    variables for which all of [predicates] are true. *)

val flag : Common.Id.t -> bool t
(** [flag id] evaluates the (potentially stochastic) flag with ID [id]. *)

(** {3 Lifting generators} *)

val lift_opt_gen : 'a Opt_gen.t -> 'a t
(** [lift_opt_gen g] lifts the fallible generator [g] to the generator monad. *)

val lift_quickcheck : 'a Base_quickcheck.Generator.t -> 'a t
(** [lift_quickcheck g] lifts the quickcheck generator [g] to the generator
    monad. *)

(** {2 Running generators} *)

val run : 'a t -> ctx:Context.t -> 'a Or_error.t
(** [run g ~ctx] runs [g] in [ctx]. It can fail if, for instance, any of the
    fallible generators lifted into [g] fail. *)
