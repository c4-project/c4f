(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Mini-model: lvalues.

    Lvalues, informally, represent a location to which we can store a
    value.  In the mini-model, they are always either variable names,
    or dereferences of other lvalues.

    Not to be confused with the more general idea of 'addresses',
    which can either be lvalues or address-of modifications of other
    addresses, and are used where things expect pointers.
*)

open Core_kernel
open Utils

type t [@@deriving sexp, eq, quickcheck]
(** Opaque type of lvalues. *)

(** Note that the default quickcheck instance generates random lvalues
   without constraint. *)

(** {3 Constructors} *)

val variable : C_identifier.t -> t
(** [variable id] constructs an lvalue pointing to variable [id].
    It doesn't do any validation. *)

val deref : t -> t
(** [deref lvalue] constructs a dereference ([*]) of another lvalue
    [lvalue].It doesn't do any validation. *)

val on_value_of_typed_id : id:C_identifier.t -> ty:Mini_type.t -> t
(** [on_value_of_typed_id ~id ~ty] constructs an lvalue with underlying
    variable [id] and the right level of indirection to convert from
    a variable of type [ty] to a primitive value.

    For example, if [ty] is a pointer type, the lvalue will become a
    dereference. *)

(** {3 Accessors} *)

val reduce
  :  t
  -> variable:(C_identifier.t -> 'a)
  -> deref:('a -> 'a)
  -> 'a
(** [reduce lvalue ~variable ~deref] applies [variable] on the
    underlying variable of [lvalue], then recursively applies [deref]
    to the result for each layer of indirection in the lvalue. *)

val is_deref : t -> bool
(** [is_deref lvalue] returns [true] if [lvalue] is a dereference of
    another [lvalue], and [false] otherwise. *)

module On_identifiers
  : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = C_identifier.t
(** Traversing over identifiers in lvalues. *)

include Mini_intf.S_has_underlying_variable with type t := t
(** We can get to the variable name inside an lvalue. *)

include Mini_intf.S_type_checkable with type t := t
(** Type-checking for lvalues. *)

(** {3 Generating random lvalues} *)

module Quickcheck_generic
    (Id : Quickcheck.S with type t := C_identifier.t)
: sig
  type nonrec t = t [@@deriving sexp_of]
  include Quickcheck.S with type t := t
end
(** Generates random lvalues, parametrised on a given identifier
    generator. *)

module Quickcheck_on_env (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of]
  include Quickcheck.S with type t := t
end
(** Generates random lvalues, constrained over the variables
    in the given environment. *)

module Quickcheck_int_values (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of]
  include Quickcheck.S with type t := t
end
(** Generates random lvalues, constrained over the variables in
    the given environment; each lvalue has a non-atomic-integer
    value type. *)
