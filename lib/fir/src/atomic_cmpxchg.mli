(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** Atomic compare-exchange operations.

    Atomic compare-exchanges can appear in statement position (where the
    boolean output is ignored) or expression position (where it isn't).
    Because of the possibility of them being in expressions, most of this
    module is parametric on an expression type to avoid cycles. *)

(** {1 Strength of compare-exchanges}

    As usual with C11, there are two types of compare-exchange: weak, which
    is allowed to fail spuriously, and strong, which isn't. *)
module Strength : sig
  (** Enumeration of compare-exchange strengths. *)
  type t =
    | Strong  (** Strong compare-exchanges. *)
    | Weak  (** Weak compare-exchanges. *)

  include Utils.Enum_types.Extension_table with type t := t
end

(** {1 Compare-exchanges} *)

(** Type of compare-exchanges, parametrised on expressions.contents

    Compare-exchanges can be quickchecked, given appropriate expression
    endpoints. While this quickcheck generates appropriate memory orders, it
    will use random addresses and strengths, and is unsuitable for
    type-safe/environment-safe generation. *)
type 'e t =
  { obj: Address.t
  ; expected: Address.t
  ; desired: 'e
  ; strength: Strength.t
  ; succ: Mem_order.t
  ; fail: Mem_order.t }
[@@deriving sexp, accessors, compare, equal, quickcheck]

(** {2 Traversal primitives} *)

(** Primitive building block for producing traversals over atomic cmpxchg.

    For module recursion reasons, we don't provide actual traversals here;
    see {!Expression}. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       'a t
    -> obj:(Address.t -> Address.t Ap.t)
    -> expected:(Address.t -> Address.t Ap.t)
    -> desired:('a -> 'b Ap.t)
    -> strength:(Strength.t -> Strength.t Ap.t)
    -> succ:(Mem_order.t -> Mem_order.t Ap.t)
    -> fail:(Mem_order.t -> Mem_order.t Ap.t)
    -> 'b t Ap.t
  (** [bmap x ~obj ~expected ~desired ~strength ~succ ~fail] applies the
      respective applicative computations to each part of [x]. *)
end

(** {1 Interface implementations} *)

include Expression_types.S_atomic with type 'e t := 'e t
