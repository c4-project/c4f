(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Classifying statements.

    This module provides several functions and enumerations for checking the
    'class' of a statement.  These are useful for, for instance, constructing
    path filters that must terminate in specific statements.

    (The name 'class' nods to the fact that, were ACT implemented in an object
     oriented language, these enumerations would literally be the class
     hierarchy of the statement type system.

    New classes will be added as and when other parts of ACT depend on them.
    To allow this to happen smoothly, the various [classify] functions can
    choose to return [None] when asked for specific information. *)

(** {1 Atomic classes} *)

module Atomic : sig
  type t =
  | Store
    [@@deriving equal, sexp]
    (** Enumeration of atomic actions. *)

  val classify : Atomic_statement.t -> t option
  (** [classify s] tries to classify [s]. *)

  val matches : t -> template:t -> bool
  (** [matches clazz ~template] checks whether [clazz] matches [template].
      Holes in [template] match any corresponding class in [clazz], but not
      vice versa. *)
end

(** {1 Atomic classes} *)
module Prim : sig
  type t =
  | Atomic of Atomic.t option
    [@@deriving equal, sexp]
    (** Enumeration of atomic actions. *)

  val classify : Prim_statement.t -> t option
  (** [classify s] tries to classify [s]. *)

  val matches : t -> template:t -> bool
  (** [matches clazz ~template] checks whether [clazz] matches [template].
      Holes in [template] match any corresponding class in [clazz], but not
      vice versa. *)
end

(** {1 Top-level statement classes} *)

type t =
  | Prim of Prim.t option
  | If
  [@@deriving equal, sexp]
(** Enumeration of top-level statements. *)

val classify : 'e Statement.t -> t option
(** [classify s] tries to classify [s]. *)

val matches : t -> template:t -> bool
(** [matches clazz ~template] checks whether [clazz] matches [template].
    Holes in [template] match any corresponding class in [clazz], but not
    vice versa. *)
