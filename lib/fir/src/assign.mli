(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A non-atomic assignment. *)

open Import

(** {1 Sources}

    FIR assignments abstract over several different types of non-atomic
    assignment-type statement. *)

module Source : sig
  (** Type of sources. *)
  type t = Dec | Inc | Expr of Expression.t
  [@@deriving accessors, sexp, compare, equal]

  val exprs : ('i, Expression.t, t, [< Accessor.many]) Accessor.t
  (** [exprs] focuses on any top-level expressions inside a source. *)

  (** Lifts an integer expression generator to a source generator. *)
  module Quickcheck_int
      (_ : Utils.My_quickcheck.S_with_sexp with type t := Expression.t) :
    Utils.My_quickcheck.S_with_sexp with type t = t

  (** Lifts a Boolean expression generator to a source generator. *)
  module Quickcheck_bool
      (_ : Utils.My_quickcheck.S_with_sexp with type t := Expression.t) :
    Utils.My_quickcheck.S_with_sexp with type t = t
end

(** {1 Assignments proper} *)

(** Opaque type of non-atomic assignments. *)
type t [@@deriving sexp, compare, equal]

(** {2 Constructors} *)

val make : dst:Lvalue.t -> src:Source.t -> t
(** [make ~dst ~src] constructs an assignment of [src] to [dst]. *)

val ( @= ) : Lvalue.t -> Expression.t -> t
(** [dst @= src] is shorthand for assigning the expression [src] to [dst]. *)

(** {2 Accessors} *)

val dst : ('i, Lvalue.t, t, [< Accessor.field]) Accessor.t
(** [dst] focuses on destination lvalues. *)

val src : ('i, Source.t, t, [< Accessor.field]) Accessor.t
(** [src] focuses on sources. *)

val exprs : ('i, Expression.t, t, [< Accessor.many]) Accessor.t
(** [exprs] focuses on any top-level expressions inside an assignment. *)

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in assignments. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over expressions in assignments. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t

(** Traversing over lvalues in assignments. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {2 Low-level quickchecking}

    See {!C4f_fir_gen}. *)

val quickcheck_observer : t Q.Observer.t
(** [quickcheck_observer] is a generic quickcheck observer for assigns. *)

(** Low-level building block for quickcheck generators on assigns. *)
module Quickcheck_generic
    (_ : Utils.My_quickcheck.S_with_sexp with type t := Source.t)
    (_ : Utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) :
  Utils.My_quickcheck.S_with_sexp with type t = t
