(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A (strong, explicit) atomic compare-exchange operation. *)

type t [@@deriving sexp, equal]

(** {3 Constructors} *)

val make :
     obj:Address.t
  -> expected:Address.t
  -> desired:Expression.t
  -> succ:Mem_order.t
  -> fail:Mem_order.t
  -> t
(** [make ~obj ~expected ~desired ~succ ~fail] constructs an explicit strong
    compare-exchange with object [obj], expected value store [expected],
    desired final value [desired], and memory orders [succ] on success and
    [fail] on failure. *)

(** {3 Accessors} *)

val obj : t -> Address.t
(** [obj cmpxchg] gets [cmpxchg]'s object address (the main target of the
    operation). *)

val expected : t -> Address.t
(** [expected cmpxchg] gets [cmpxchg]'s expected address (the location that
    holds the expected value, and receives the actual value). *)

val desired : t -> Expression.t
(** [desired cmpxchg] gets [cmpxchg]'s desired-value expression (written to
    the object on success). *)

val succ : t -> Mem_order.t
(** [succ cmpxchg] gets [cmpxchg]'s memory order on success. *)

val fail : t -> Mem_order.t
(** [fail cmpxchg] gets [cmpxchg]'s memory order on failure. *)

(** {3 Traversals} *)

(** Traversing over atomic-action addresses in atomic compare-exchanges. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over lvalues in atomic compare-exchanges. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t
