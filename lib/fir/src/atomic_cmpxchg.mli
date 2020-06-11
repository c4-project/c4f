(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** A (strong, explicit) atomic compare-exchange operation.

    Atomic compare-exchanges can appear in statement position (where the
    boolean output is ignored) or expression position (where it isn't).
    Because of the possibility of them being in expressions, most of this
    module is parametric on an expression type to avoid cycles. *)

(** Opaque type of compare-exchanges, parametrised on expressions.

    Compare-exchanges can be quickchecked, given appropriate expression
    endpoints. While this quickcheck generates appropriate memory orders, it
    will use random addresses, and is unsuitable for
    type-safe/environment-safe generation. *)
type 'e t [@@deriving sexp, compare, equal, quickcheck]

(** {1 Constructors} *)

val make :
     obj:Address.t
  -> expected:Address.t
  -> desired:'e
  -> succ:Mem_order.t
  -> fail:Mem_order.t
  -> 'e t
(** [make ~obj ~expected ~desired ~succ ~fail] constructs an explicit strong
    compare-exchange with object [obj], expected value store [expected],
    desired final value [desired], and memory orders [succ] on success and
    [fail] on failure. *)

(** {1 Accessors} *)

val obj : _ t -> Address.t
(** [obj cmpxchg] gets [cmpxchg]'s object address (the main target of the
    operation). *)

val expected : _ t -> Address.t
(** [expected cmpxchg] gets [cmpxchg]'s expected address (the location that
    holds the expected value, and receives the actual value). *)

val desired : 'e t -> 'e
(** [desired cmpxchg] gets [cmpxchg]'s desired-value expression (written to
    the object on success). *)

val succ : _ t -> Mem_order.t
(** [succ cmpxchg] gets [cmpxchg]'s memory order on success. *)

val fail : _ t -> Mem_order.t
(** [fail cmpxchg] gets [cmpxchg]'s memory order on failure. *)

(** {1 Traversal primitives} *)

(** Primitive building block for producing traversals over atomic cmpxchg.

    For module recursion reasons, we don't provide actual traversals here;
    see {!Expression}. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       'a t
    -> obj:(Address.t -> Address.t Ap.t)
    -> expected:(Address.t -> Address.t Ap.t)
    -> desired:('a -> 'b Ap.t)
    -> succ:(Mem_order.t -> Mem_order.t Ap.t)
    -> fail:(Mem_order.t -> Mem_order.t Ap.t)
    -> 'b t Ap.t
end

(** {1 Interface implementations} *)

include Expression_types.S_atomic with type 'e t := 'e t
