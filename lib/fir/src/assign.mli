(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A non-atomic assignment. *)

(** {1 Sources}

    FIR assignments abstract over several different types of non-atomic
    assignment-type statement. *)

module Source : sig
  (** Type of sources. *)
  type t = Dec | Inc | Expr of Expression.t
  [@@deriving accessors, sexp, compare, equal]

  val exprs : ('i, Expression.t, t, [< Accessor.many]) Accessor.Simple.t
  (** [exprs] focuses on any top-level expressions inside a source. *)
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

val dst : ('i, Lvalue.t, t, [< Accessor.field]) Accessor.Simple.t
(** [dst] focuses on destination lvalues. *)

val src : ('i, Source.t, t, [< Accessor.field]) Accessor.Simple.t
(** [src] focuses on sources. *)

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
