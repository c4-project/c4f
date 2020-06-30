(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Classifying expressions.

    This module provides several functions and enumerations for checking the
    'class' of an expression. These are useful for, for instance,
    constructing path filters that require reject the presence of particular
    expressions.

    See {!Class} for a more general discussion. *)

(** {1 Top-level expression classes} *)

(** Enumeration of expressions. *)
type t =
  | Constant  (** This expression is a constant of some form. *)
  | Address
  | Atomic of Atomic_class.t option
      (** This expression is an atomic action of some form. *)
  | Bop of Op.Binary.t option
      (** This expression is a binary operation of some form. *)
  | Uop of Op.Unary.t option
      (** This expression is a unary operation of some form. *)
[@@deriving compare, equal, sexp]

include Class_types.S with type t := t and type 'e elt := Expression.t

include Class_types.S_ext with type t := t and type 'e elt := Expression.t
