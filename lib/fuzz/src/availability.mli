(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Availability checks and helpers for constructing them. *)

open Base
open Import

(** {1 Context for availability checks}

    Availability checks depend on three pieces of information:

    - a snapshot of the current test subject;
    - a snapshot of the current fuzzer state;
    - the parameter map supplied to the top-level fuzz runner. *)
module Context : sig
  (** Type of availability contexts. *)
  type t = {subject: Subject.Test.t; state: State.t; params: Param_map.t}
  [@@deriving accessors, make]
end

(** {1 Availability checks} *)

(** Reader monad useful for constructing availability checks. *)
module M : sig
  include
    Utils.Reader_types.S
      with type 'r Inner.t = 'r Or_error.t
       and type ctx = Context.t

  val lift_acc :
       (unit -> 'a -> 'b, unit -> ctx -> 'c, [> getter]) Accessor.General.t
    -> 'a t
  (** [lift_acc acc] is shorthand for [lift (Accessor.get acc)]. *)

  val lift_state : (State.t -> 'a) -> 'a t
  (** [lift_state f] lifts a function from states to values into the check
      monad. *)

  val param : Common.Id.t -> int t
  (** [param id] retrieves the integer fuzzer parameter with type [id]. *)
end

(** Type of availability checks. *)
type t = bool M.t

(** Availability checks can be composed together monoidically. *)
include Container.Summable with type t := t

(** {2 Common checks} *)

val always : t
(** [always] is an availability check that always returns true.

    Note that, in 99.9% of the cases, you probably want {!has_threads}
    instead. *)

val has_threads : t
(** [has_threads] is an availability check that returns true when at least
    one thread exists. *)

val is_filter_constructible : Path_filter.t -> kind:Path_kind.t -> t
(** [is_filter_constructible filter ~kind] holds if returns true if at least
    one path of kind [kind] is constructible from the subject that satisfies
    [filter]. *)

val has_variables : predicates:(Var.Record.t -> bool) list -> t
(** [has_variables ~predicates] holds if there exists at least one variable
    in the current map that satisfies all of [predicates]. *)

val in_var_cap : after_adding:int -> t
(** [in_var_cap ~after_adding] holds if, after adding [after_adding]
    variables, we are within the variable cap. If [after_adding] is zero or
    negative, this will return [true] regardless of whether the number of
    variables already exceeds the cap. *)
