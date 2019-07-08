(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Mini-model: lvalues.

    Lvalues, informally, represent a location to which we can store a value.
    In the mini-model, they are always either variable names, or
    dereferences of other lvalues.

    Not to be confused with the more general idea of 'addresses', which can
    either be lvalues or address-of modifications of other addresses, and
    are used where things expect pointers. *)

open Base

type t [@@deriving sexp, compare, equal, quickcheck]
(** Opaque type of lvalues. *)

(** Note that the default quickcheck instance generates random lvalues
    without constraint. *)

(** {3 Constructors} *)

val variable : Act_common.C_id.t -> t
(** [variable id] constructs an lvalue pointing to variable [id]. It doesn't
    do any validation. *)

val deref : t -> t
(** [deref lvalue] constructs a dereference ([*]) of another lvalue
    [lvalue]. It doesn't do any validation. *)

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

include
  Types.S_has_underlying_variable with type t := t
(** We can get to the variable name inside an lvalue. *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for lvalues. *)

(** {3 Generating random lvalues} *)

(** Generates random lvalues, parametrised on a given identifier generator. *)
module Quickcheck_generic
    (Id : Act_utils.My_quickcheck.S_with_sexp
            with type t := Act_common.C_id.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end

(** Generates random lvalues, constrained over the variables in the given
    environment. *)
module Quickcheck_on_env (E : Env_types.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end

(** Generates random lvalues, constrained over the variables in the given
    environment; each lvalue has a non-atomic-integer value type. *)
module Quickcheck_int_values (E : Env_types.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end
