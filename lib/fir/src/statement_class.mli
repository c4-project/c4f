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
    | Early_out of Early_out.t option
        (** An early-out with the given specific classification. *)
    | Label  (** A label. *)
  [@@deriving compare, equal, sexp]

  include Class_types.S with type t := t and type 'e elt := Prim_statement.t
end

(** {1 Flow classes} *)
module Flow : sig
  (** Enumeration of flow statement classes. *)
  type t =
    | For
    | Lock of Flow_block.Lock.t option
    | While of Flow_block.While.t option
  [@@deriving compare, equal, sexp]

  include
    Class_types.S
      with type t := t
       and type 'e elt := ('e, 'e Statement.t) Flow_block.t
end

(** {1 Top-level statement classes} *)

(** Enumeration of top-level statements. *)
type t =
  | Prim of Prim.t option
  | If  (** This statement is an if statement. *)
  | Flow of Flow.t option  (** This statement is a flow block. *)
[@@deriving compare, equal, sexp]

include Class_types.S with type t := t and type 'e elt := 'e Statement.t

include Class_types.S_ext with type t := t and type 'e elt := 'e Statement.t

(** {2 Convenience constructors} *)

val atomic : ?specifically:Atomic_class.t -> unit -> t
(** [atomic ?specifically ()] constructs an atomic class, optionally with the
    details in [specifically]. *)

val while_loop : ?specifically:Flow_block.While.t -> unit -> t
(** [while_loop ?specifically ()] constructs a while-loop class, optionally
    with the details in [specifically]. *)

val lock_block : ?specifically:Flow_block.Lock.t -> unit -> t
(** [lock_block ?specifically ()] constructs a lock-block class, optionally
    with the details in [specifically]. *)

val label : t
(** [label] is a class that matches label statements. *)
