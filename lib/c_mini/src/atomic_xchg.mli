(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** An (explicit) atomic exchange operation.

    Atomic exchanges can appear in statement position (where the output is
    ignored and the effect is broadly that of a store) or expression position
    (where it isn't). Because of the possibility of them being in
    expressions, most of this module is parametric on an expression type to
    avoid cycles. *)

(** Opaque type of exchanges, parametrised on expressions.

    Exchanges can be quickchecked, given appropriate expression endpoints.
    While this quickcheck generates appropriate memory orders, it will use
    random addresses, and is unsuitable for type-safe/environment-safe
    generation. *)
type 'e t [@@deriving sexp, compare, equal, quickcheck]

(** {1 Constructors} *)

val make : obj:Address.t -> desired:'e -> mo:Mem_order.t -> 'e t
(** [make ~obj ~desired ~mo] constructs an explicit exchange with object
    [obj], desired final value [desired], and memory order [mo]. *)

(** {1 Accessors} *)

val obj : _ t -> Address.t
(** [obj xchg] gets [xchg]'s object address (the main target of the
    operation). *)

val desired : 'e t -> 'e
(** [desired xchg] gets [xchg]'s desired-value expression (written to the
    object on success). *)

val mo : _ t -> Mem_order.t
(** [mo xchg] gets [xchg]'s memory order. *)

(** {1 Traversal primitives} *)

(** Primitive building block for producing traversals over atomic xchg.

    For module recursion reasons, we don't provide actual traversals here;
    see {!Expression}. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       'a t
    -> obj:(Address.t -> Address.t Ap.t)
    -> desired:('a -> 'b Ap.t)
    -> mo:(Mem_order.t -> Mem_order.t Ap.t)
    -> 'b t Ap.t
end

(** {1 Interface implementations} *)

include Expression_types.S_atomic with type 'e t := 'e t
