(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Classifying statements.

    This module provides several functions and enumerations for checking the
    'class' of a statement. These are useful for, for instance, constructing
    path filters that must terminate in specific statements.

    (The name 'class' nods to the fact that, were ACT implemented in an
    object oriented language, these enumerations would literally be the class
    hierarchy of the statement type system.

    New classes will be added as and when other parts of ACT depend on them.
    To allow this to happen smoothly, the various [classify] functions can
    choose to return [None] when asked for specific information. *)

(** {1 Primitive classes} *)
module Prim : sig
  (** Enumeration of primitive statement classes. *)
  type t =
    | Atomic of Atomic_class.t option
        (** An atomic action with the given atomic class information. *)
    | Label  (** A label. *)
  [@@deriving compare, equal, sexp]

  val classify : Prim_statement.t -> t option
  (** [classify s] tries to classify [s]. *)

  val matches : t -> template:t -> bool
  (** [matches clazz ~template] checks whether [clazz] matches [template].
      Holes in [template] match any corresponding class in [clazz], but not
      vice versa. *)
end

(** {1 Flow classes} *)
module Flow : sig
  (** Enumeration of flow statement classes. *)
  type t =
    | Lock of Flow_block.Lock.t option
    | While of Flow_block.While.t option
  [@@deriving compare, equal, sexp]

  val classify : (_, _) Flow_block.t -> t option
  (** [classify s] tries to classify [s]. *)

  val matches : t -> template:t -> bool
  (** [matches clazz ~template] checks whether [clazz] matches [template].
      Holes in [template] match any corresponding class in [clazz], but not
      vice versa. *)
end

(** {1 Top-level statement classes} *)

(** Enumeration of top-level statements. *)
type t =
  | Prim of Prim.t option
  | If  (** This statement is an if statement. *)
  | Flow of Flow.t option  (** This statement is a flow block. *)
[@@deriving compare, equal, sexp]

val classify : 'e Statement.t -> t option
(** [classify s] tries to classify [s]. *)

val matches : t -> template:t -> bool
(** [matches clazz ~template] checks whether [clazz] matches [template].
    Holes in [template] match any corresponding class in [clazz], but not
    vice versa. *)

val matches_any : t -> templates:t list -> bool
(** [matches_any clazz ~templates] checks whether [clazz] {!matches} any of
    the templates in [templates]. *)

val statement_matches_any : 'e Statement.t -> templates:t list -> bool
(** [statement_matches_any stm ~templates] checks whether [stm]'s class
    directly {!matches} any of the templates in [templates]. *)

val statement_recursively_matches_any :
  'e Statement.t -> templates:t list -> bool
(** [statement_recursively_matches_any stm ~templates] checks whether [stm]'s
    class recursively {!matches} any of the templates in [templates]. *)

val count_matches : 'meta Statement.t -> templates:t list -> int
(** [count_matches stm ~templates] counts the recursive number of times that
    any class in [templates] matches a statement in [stm]. *)

(** {2 Convenience constructors} *)

val atomic : ?specifically:Atomic_class.t -> unit -> t
(** [atomic ?specifically ()] constructs an atomic class, optionally with the
    details in [specifically]. *)

val while_loop : ?specifically:Flow_block.While.t -> unit -> t
(** [while_loop ?specifically ()] constructs a while-loop class, optionally
    with the details in [specifically]. *)

val label : t
(** [label] is a class that matches label statements. *)
