(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: lvalues.

    Lvalues, informally, represent a location to which we can store a value.
    In the mini-model, they are always either variable names, or dereferences
    of other lvalues.

    Not to be confused with the more general idea of 'addresses', which can
    either be lvalues or address-of modifications of other addresses, and are
    used where things expect pointers. *)

open Base

(** Opaque type of lvalues. *)
type t [@@deriving sexp, compare, equal, quickcheck]

(** Note that the default quickcheck instance generates random lvalues
    without constraint. *)

(** {3 Constructors} *)

val variable : Act_common.C_id.t -> t
(** [variable id] constructs an lvalue pointing to variable [id]. It doesn't
    do any validation. *)

val of_variable_str_exn : string -> t
(** [of_variable_str_exn vs] tries to construct an lvalue over a variable
    whose name is the string [vs]. It fails with an exception if [vs] is an
    invalid C identifier. This function is for use mainly in testing, and
    shouldn't be used on user-supplied C code. *)

val deref : t -> t
(** [deref lvalue] constructs a dereference ([*]) of another lvalue [lvalue].
    It doesn't do any validation. *)

val un_deref : t -> t Or_error.t
(** [un_deref lvalue] tries to remove one layer of dereferencing from
    [lvalue]. It fails if [lvalue] is already a variable type. *)

val on_value_of_typed_id : id:Act_common.C_id.t -> ty:Type.t -> t
(** [on_value_of_typed_id ~id ~ty] constructs an lvalue with underlying
    variable [id] and the right level of indirection to convert from a
    variable of type [ty] to a primitive value.

    For example, if [ty] is a pointer type, the lvalue will become a
    dereference. *)

(** {3 Accessors} *)

val reduce :
  t -> variable:(Act_common.C_id.t -> 'a) -> deref:('a -> 'a) -> 'a
(** [reduce lvalue ~variable ~deref] applies [variable] on the underlying
    variable of [lvalue], then recursively applies [deref] to the result for
    each layer of indirection in the lvalue. *)

val is_deref : t -> bool
(** [is_deref lvalue] returns [true] if [lvalue] is a dereference of another
    [lvalue], and [false] otherwise. *)

(** Traversing over identifiers in lvalues. *)
module On_identifiers :
  Travesty.Traversable_types.S0
    with type t := t
     and type Elt.t = Act_common.C_id.t

(** We can get to the variable name inside an lvalue. *)
include Types.S_has_underlying_variable with type t := t

(** Type-checking for lvalues. *)
include Types.S_type_checkable with type t := t

(** {3 Safe accessors}

    These accessors return an error if the address isn't in the right shape. *)

val as_variable : t -> Act_common.C_id.t Or_error.t
(** [as_variable lv] returns [lv], then returns it as a variable ID if it is
    one, or an error otherwise. *)

(** {3 Generating random lvalues}

    The default quickcheck instance random lvalues without constraint. We
    also provide several modules with more specific restrictions. Most of
    these are in {!Lvalue_gen}, but we expose their common ancestor,
    {!Quickcheck_generic}, here by necessity. *)

(** Generates random lvalues, parametrised on a given identifier generator. *)
module Quickcheck_generic
    (Id : Act_utils.My_quickcheck.S_with_sexp
            with type t := Act_common.C_id.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end
