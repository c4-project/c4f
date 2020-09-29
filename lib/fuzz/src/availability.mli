(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Availability checks and helpers for constructing them. *)

open Base

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
module M :
  Act_utils.Reader_types.S
    with type 'r Inner.t = 'r Or_error.t
     and type ctx = Context.t

(** Type of availability checks. *)
type t = bool M.t

(** Availability checks can be composed together monoidically. *)
include Container.Summable with type t := t

val lift_state : (State.t -> 'a) -> 'a M.t
(** [lift_state f] lifts a function from states to values into the check
    monad. *)

(** {2 Common checks} *)

val always : t
(** [always] is an availability check that always returns true.

    Note that, in 99.9% of the cases, you probably want {!has_threads}
    instead. *)

val has_threads : t
(** [has_threads] is an availability check that returns true when at least
    one thread exists. *)

val is_filter_constructible : Path_filter.t -> kind:Path_kind.t -> t
(** [is_filter_constructible filter ~kind] is an availability check that
    returns true if at least one path of kind [kind] is constructible from
    the subject that satisfies [filter]. *)

val has_variables : predicates:(Var.Record.t -> bool) list -> t
(** [has_variables ~predicates] is an availability check that returns true if
    there exists at least one variable in the current map that satisfies all
    of [predicates]. *)
