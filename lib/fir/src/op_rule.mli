(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: types and support for operator algebraic rules

    These are used in {!Op_types} and {!Op}. *)

(** {1 Rule inputs} *)

module In : sig
  (** {2 Locations of constants in constant rules} *)

  module Dir : sig
    type t = Left | Right [@@deriving accessors]
  end

  type t = Const of Dir.t * Constant.t | Refl [@@deriving accessors]

  (** {2 Common input shorthands} *)

  val zero : Dir.t -> t
  (** [zero d] is short for [Const (d, int 0)]. *)

  val minus_one : Dir.t -> t
  (** [minus_one d] is short for [Const (d, int (-1))]. *)

  val zero' : Dir.t -> ('a, unit, t, [< Accessor.variant]) Accessor.t
  (** [zero' d] can be used to match on, or construct, a zero input. *)

  val true_ : Dir.t -> t
  (** [true_ d] is short for [Const (d, bool True)]. *)

  val false_ : Dir.t -> t
  (** [false_ d] is short for [Const (d, bool False)]. *)
end

(** {1 Rule outputs} *)

module Out : sig
  type t = Const of Constant.t | Idem [@@deriving accessors]

  (** {2 Common output shorthands} *)

  val zero : t
  (** [zero] is short for [Const (Int 0)]. *)

  val true_ : t
  (** [true_] is short for [Const (bool True)]. *)

  val false_ : t
  (** [false_] is short for [Const (bool False)]. *)
end

(** {1 The rule type} *)

(** Type of rules. *)
type t = {in_: In.t; out_: Out.t} [@@deriving sexp]

val ( @-> ) : In.t -> Out.t -> t
(** [i @-> o] constructs a rule from i to o. *)

(** {2 Common rules} *)

(** {2 Searching for rules} *)

val single_in_matching :
  Out.t -> ('i, In.t, t, [< Accessor.optional_getter]) Accessor.t
(** [in_matching out_] accesses the input criteria for a rule, provided that
    its output criteria is [out_]. *)

val in_matching :
  Out.t -> ('i, In.t, t list, [< Accessor.many_getter]) Accessor.t
(** [in_matching out_] accesses the input criteria for all rules in a rule
    list whose output criteria is [out_]. *)

val has_in_out_matching :
     (unit, 'a, In.t, Accessor.many_getter) Accessor.t
  -> Out.t
  -> t list
  -> bool
(** [has_in_out_matching in_acc out xs] is [true] if, and only if, [xs] has
    at least one rule ending in [out] and for which [in_acc] produces values. *)
