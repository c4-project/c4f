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

(** Mini model: addresses (a lvalue, or reference thereto). *)

type t [@@deriving sexp, compare, equal, quickcheck]
(** Opaque type of addresses. *)

(** Traversing over lvalues in addresses. *)
module On_lvalues :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Lvalue.t

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
    for every layer of indirection. *)

val of_variable : Act_common.C_id.t -> t
(** [of_variable v] lifts the variable identifier [v] directly to an
    address. *)

val of_variable_ref : Act_common.C_id.t -> t
(** [of_variable_ref v] lifts the address of variable identifier [v] to an
    address (in C syntax, this would be '&v'). *)

val on_address_of_typed_id : id:Act_common.C_id.t -> ty:Type.t -> t

(** {3 Accessors} *)

val reduce : t -> lvalue:(Lvalue.t -> 'a) -> ref:('a -> 'a) -> 'a
(** [reduce addr ~address ~deref] applies [lvalue] on the underlying lvalue
    of [addr], then recursively applies [ref] to the result for each layer
    of address-taking in the address. *)

include
  Types.S_has_underlying_variable with type t := t
(** We can get to the variable name inside an address. *)

(** {3 Type-checking} *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for addresses. *)

(** {3 Generating and quickchecking}

    The default quickcheck instance random addresses without constraint. We
    also provide several modules with more *)

module Quickcheck_generic
    (Lv : Act_utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random addresses, parametrised on a given lvalue generator. *)

module Quickcheck_on_env (E : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random addresses, constrained over the variables in the given
    environment. *)

module Quickcheck_atomic_int_pointers (E : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates addresses over the given typing environment that have the type
    'atomic_int*'. *)