(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: lvalues.

    Lvalues, informally, represent a location to which we can store a value.
    In FIR, they are always either variable names, or dereferences of other
    lvalues.

    Not to be confused with the more general idea of 'addresses', which can
    either be lvalues or address-of modifications of other addresses, and are
    used where things expect pointers. *)

open Base
open Import

(** Type of lvalues. *)
type t = Variable of Common.C_id.t | Deref of t
[@@deriving sexp, accessors, compare, equal, quickcheck]

(** Note that the default quickcheck instance generates random lvalues
    without constraint. *)

(** {1 Constructors} *)

val of_variable_str_exn : string -> t
(** [of_variable_str_exn vs] tries to construct an lvalue over a variable
    whose name is the string [vs]. It fails with an exception if [vs] is an
    invalid C identifier. This function is for use mainly in testing, and
    shouldn't be used on user-supplied C code. *)

val un_deref : t -> t Or_error.t
(** [un_deref lvalue] tries to remove one layer of dereferencing from
    [lvalue]. It fails if [lvalue] is already a variable type. *)

val on_value_of_typed_id : Type.t C4f_common.C_named.t -> t
(** [on_value_of_typed_id tid] constructs an lvalue with underlying variable
    [name tid] and the right level of indirection to convert from a variable
    of type [value tid] to a primitive value.

    For example, if [value tid] is a pointer type, the lvalue will become a
    dereference. *)

(** {1 Getters and accessors} *)

val reduce :
  t -> variable:(C4f_common.C_id.t -> 'a) -> deref:('a -> 'a) -> 'a
(** [reduce lvalue ~variable ~deref] applies [variable] on the underlying
    variable of [lvalue], then recursively applies [deref] to the result for
    each layer of indirection in the lvalue. *)

val is_deref : t -> bool
(** [is_deref lvalue] returns [true] if [lvalue] is a dereference of another
    [lvalue], and [false] otherwise. *)

(** {2 Safe getters}

    These getters return an error if the address isn't in the right shape. *)

val as_variable : t -> C4f_common.C_id.t Or_error.t
(** [as_variable lv] returns [lv], then returns it as a variable ID if it is
    one, or an error otherwise. *)

(** We can get to the variable name inside an lvalue. *)
include Types.S_has_underlying_variable with type t := t

(** Type-checking for lvalues. *)
include Types.S_type_checkable with type t := t

(** {1 Generating random lvalues}

    The default quickcheck instance random lvalues without constraint. We
    also provide several modules with more specific restrictions. Most of
    these are in {!Lvalue_gen}, but we expose their common ancestor,
    {!Quickcheck_generic}, here by necessity. *)

(** Generates random lvalues, parametrised on a given identifier generator. *)
module Quickcheck_generic
    (Id : C4f_utils.My_quickcheck.S_with_sexp
            with type t := C4f_common.C_id.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end
