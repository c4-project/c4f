(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: types and support for operator algebraic rules

    These are used in {!Op_types} and {!Op}. *)

(** {1 The rule type} *)

(** Type of outcomes of algebraic questions such as 'does [x op x] return
    a particular statically-known value?'. *)
type t

(** {2 Accessors} *)
  
val idem : ('a, unit, t, [< Accessor.variant]) Accessor.Simple.t
(** [idem] focuses on whether a rule specifies idempotence. *)

val zero : ('a, unit, t, [< Accessor.variant]) Accessor.Simple.t
(** [zero] focuses on whether a rule specifies that the return is always zero. *)

val unknown : ('a, unit, t, [< Accessor.variant]) Accessor.Simple.t
(** [unknown] focuses on whether a rule specifies that the return is unknown. *)

(** {2 Queries} *)

val is_idem : t -> bool
(** [is_idem x] gets whether [x] is idempotence. *)

val is_zero : t -> bool
(** [is_zero x] gets whether [x] is constant zero. *)
