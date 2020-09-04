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

val const : ('a, Constant.t, t, [< Accessor.variant]) Accessor.Simple.t
(** [const] focuses on whether a rule specifies that the return is always a constant. *)

val unknown : ('a, unit, t, [< Accessor.variant]) Accessor.Simple.t
(** [unknown] focuses on whether a rule specifies that the return is unknown. *)

(** {2 Constructors} *)

val mk_idem : unit -> t
(** [mk_idem ()] is an idempotence rule. *)

val mk_const : Constant.t -> t
(** [mk_const k] is a rule that suggests the result of an op is always [k]. *)

val mk_true : unit -> t
(** [mk_true ()] is an always-true rule. *)

val mk_false : unit -> t
(** [mk_false ()] is an always-false rule. *)

val mk_zero : unit -> t
(** [mk_zero ()] is an always-zero rule. *)

val mk_unknown : unit -> t
(** [mk_unknown ()] is an absence of known rule. *)

(** {2 Queries} *)

val is_idem : t -> bool
(** [is_idem x] gets whether [x] is idempotence. *)

val is_true : t -> bool
(** [is_true x] gets whether [x] is constant true. *)

val is_false : t -> bool
(** [is_false x] gets whether [x] is constant false. *)

val is_zero : t -> bool
(** [is_zero x] gets whether [x] is constant zero. *)
