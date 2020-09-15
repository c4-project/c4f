(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: addresses (a lvalue, or reference thereto). *)

open Base
open Import

(** Type of addresses. *)
type t = Lvalue of Lvalue.t | Ref of t
[@@deriving accessors, sexp, compare, equal, quickcheck]

include Comparable.S with type t := t

(** {1 Constructors} *)

val ref_lvalue : Lvalue.t -> t
(** [ref_lvalue lv] constructs a &-reference to [lv]. If [lv] is already a
    dereference, it strips one layer of dereferencing instead of creating a
    new layer of indirection. *)

val ref_normal : t -> t
(** [ref_normal t] constructs a &-reference to [t], but uses {{!ref_lvalue}
    ref_lvalue} if [t] is an lvalue. *)

val normalise : t -> t
(** [normalise addr] reconstructs [addr], using {{!ref_normal} ref_normal}
    for every layer of indirection.

    Note that this operation can remove information about which operations
    are safe to perform on an address. For example, `&*foo` implies that
    `foo` is a pointer type (and therefore `*foo` is valid): normalisation
    will reduce it to `foo`, which doesn't. *)

val deref : t -> t
(** [deref addr] tries to strip a level of reference from [addr]. If the
    normalised form of [addr] is an lvalue, it returns the result of directly
    dereferencing from that lvalue instead. *)

val on_address_of_typed_id : Type.t Common.C_named.t -> t

val of_id_in_env : Env.t -> id:Common.C_id.t -> t Or_error.t

val of_variable_str_exn : string -> t
(** [of_variable_str_exn vs] is shorthand for applying {!of_variable} to the
    result of converting [vs] from a string to a C identifier. It will fail
    with an exception if [vs] is not a valid C identifier, and so this
    function should not be used on user-input data. *)

(** {1 Accessors and various traversals} *)

val reduce : t -> lvalue:(Lvalue.t -> 'a) -> ref:('a -> 'a) -> 'a
(** [reduce addr ~address ~deref] applies [lvalue] on the underlying lvalue
    of [addr], then recursively applies [ref] to the result for each layer of
    address-taking in the address. *)

val lvalue_of : ('i, Lvalue.t, t, [< field]) Accessor.Simple.t
(** [lvalue_of] focuses on an address's underlying lvalue. *)

(** This is just [lvalue_of] as a traversable. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** We can get to the variable name inside an address. *)
include Types.S_has_underlying_variable with type t := t

(** {2 Variable accessors} *)

val variable : ('i, Common.C_id.t, t, [< variant]) Accessor.Simple.t
(** [variable] focuses on addresses that are just variable lvalues. *)

val variable_ref : ('i, Common.C_id.t, t, [< variant]) Accessor.Simple.t
(** [variable_ref] focuses on addresses that are references to variable
    lvalues. *)

(** {1 Safe accessors}

    These accessors return an error if the address isn't in the right shape. *)

val as_lvalue : t -> Lvalue.t Or_error.t
(** [as_lvalue addr] normalises [addr], then returns it as an {{!Lvalue.t}
    lvalue} if it is one, or an error otherwise. *)

val as_variable : t -> Common.C_id.t Or_error.t
(** [as_variable addr] normalises [addr], then returns it as a variable ID if
    it is one, or an error otherwise. *)

(** {1 Type-checking} *)

(** Type-checking for addresses. *)
include Types.S_type_checkable with type t := t

(** {1 Generating and quickchecking}

    The default quickcheck instance random addresses without constraint. We
    also provide several modules with more specific restrictions. Most of
    these are in {!Address_gen}, but we expose their common ancestor,
    {!Quickcheck_generic}, here by necessity. *)

(** Generates random addresses, parametrised on a given lvalue generator. *)
module Quickcheck_generic
    (Lv : Act_utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t

val eval_on_env : t -> env:Env.t -> Constant.t Or_error.t
(** [eval_on_env address ~env] tries to evaluate an address [lv] against the
    known-value tracking of an environment [env]. Currently, it supports only
    scalars; it rejects any ref or deref addresses. *)
