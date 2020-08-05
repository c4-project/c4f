(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Availability checks and helpers for constructing them. *)

open Base

(** {1 Context for availability checks} *)

module Context : sig
  (** Opaque type of availability contexts. *)
  type t

  (** {2 Constructors} *)

  val make :
    subject:Subject.Test.t -> param_map:Param_map.t -> state:State.t -> t
  (** [make ~subject ~param_map ~state] makes a context with the given
      components. *)

  (** {2 Accessors} *)

  val subject : t -> Subject.Test.t
  (** [subject] gets the subject at the point of the availability check. *)

  val param_map : t -> Param_map.t
  (** [param_map] gets the parameter map at the point of the availability
      check. *)

  val state : t -> State.t
  (** [param_map] gets the fuzzer state at the point of the availability
      check. *)
end

(** {1 Availability checks} *)

(** [t] is the type of availability checks. *)
type t = Context.t -> bool Or_error.t

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
(** [is_filter_constructible filter ~kind] is an availability check that
    returns true if at least one path of kind [kind] is constructible from
    the subject that satisfies [filter]. *)
