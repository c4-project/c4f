(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini model: addresses (a lvalue, or reference thereto). *)

open Base

type t [@@deriving sexp, compare, equal, quickcheck]
(** Opaque type of addresses. *)

(** Traversing over lvalues in addresses. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {3 Constructors} *)

val lvalue : Lvalue.t -> t
(** [lvalue lv] lifts an lvalue [lv] to an address. *)

val ref : t -> t
(** [ref t] constructs a &-reference to [t]. *)

val ref_lvalue : Lvalue.t -> t
(** [ref_lvalue lv] constructs a &-reference to [lv]. If [lv] is already a
    dereference, it strips one layer of dereferencing instead of creating a
    new layer of indirection. *)

val ref_normal : t -> t
(** [ref_normal t] constructs a &-reference to [t], but uses
    {{!ref_lvalue} ref_lvalue} if [t] is an lvalue. *)

val normalise : t -> t
(** [normalise addr] reconstructs [addr], using {{!ref_normal} ref_normal}
    for every layer of indirection.

    Note that this operation can remove information about which operations
    are safe to perform on an address. For example, `&*foo` implies that
    `foo` is a pointer type (and therefore `*foo` is valid): normalisation
    will reduce it to `foo`, which doesn't. *)

val deref : t -> t
(** [deref addr] tries to strip a level of reference from [addr]. If the
    normalised form of [addr] is an lvalue, it returns the result of
    directly dereferencing from that lvalue instead. *)

val of_variable : Act_common.C_id.t -> t
(** [of_variable v] lifts the variable identifier [v] directly to an
    address. *)

val of_variable_ref : Act_common.C_id.t -> t
(** [of_variable_ref v] lifts the address of variable identifier [v] to an
    address (in C syntax, this would be '&v'). *)

val on_address_of_typed_id : id:Act_common.C_id.t -> ty:Type.t -> t

val of_id_in_env :
  (module Env_types.S) -> id:Act_common.C_id.t -> t Or_error.t

(** {3 Accessors} *)

val reduce : t -> lvalue:(Lvalue.t -> 'a) -> ref:('a -> 'a) -> 'a
(** [reduce addr ~address ~deref] applies [lvalue] on the underlying lvalue
    of [addr], then recursively applies [ref] to the result for each layer
    of address-taking in the address. *)

val lvalue_of : t -> Lvalue.t
(** [lvalue_of addr] gets the underlying lvalue of [addr]. *)

include
  Types.S_has_underlying_variable with type t := t
(** We can get to the variable name inside an address. *)

(** {3 Safe accessors}

    These accessors return an error if the address isn't in the right shape. *)

val as_lvalue : t -> Lvalue.t Or_error.t
(** [as_lvalue addr] normalises [addr], then returns it as an
    {{!Lvalue.t} lvalue} if it is one, or an error otherwise. *)

val as_variable : t -> Act_common.C_id.t Or_error.t
(** [as_variable addr] normalises [addr], then returns it as a variable ID
    if it is one, or an error otherwise. *)

(** {3 Type-checking} *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for addresses. *)

(** {3 Generating and quickchecking}

    The default quickcheck instance random addresses without constraint. We
    also provide several modules with more specific restrictions. Most of
    these are in {{!Address_gen}Address_gen}, but we expose their common
    ancestor, {{!Quickcheck_generic}Quickcheck_generic}}, here by necessity. *)

module Quickcheck_generic
    (Lv : Act_utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random addresses, parametrised on a given lvalue generator. *)

val eval_on_env :
  (module Env_types.S_with_known_values) -> t -> Constant.t Or_error.t
(** [eval_on_env env address] tries to evaluate an address [lv] against the
    known-value tracking of an environment [env]. Currently, it supports
    only scalars; it rejects any ref or deref addresses. *)
