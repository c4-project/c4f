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

(** Mini-model: type system

    This is a conservative subset of C11's type system, with a few things
    that'd normally be typedefs pulled into distinct types. *)

open Core_kernel
open Act_utils

(** Primitive types. *)
module Basic : sig
  type t
  (** Opaque type of basic types. *)

  val bool : t
  (** [bool] is the (C99?) Boolean type. *)

  val int : t
  (** [int] is the int type. *)

  val atomic_bool : t
  (** [atomic_bool] is the atomic_bool type. *)

  val atomic_int : t
  (** [atomic_int] is the atomic_int type. *)

  include Enum.Extension_table with type t := t

  val to_spec : t -> [> Act_c_lang.Ast.Type_spec.t]
  (** [to_spec btype] converts a basic type to a type spec. *)

  val is_atomic : t -> bool
  (** [is_atomic btype] is true if the basic type [btype] is atomic. *)

  val to_non_atomic : t -> t Or_error.t
  (** [to_non_atomic btype] tries to get the non-atomic type corresponding
      to the atomic type [btype]. It fails if [btype] isn't atomic. *)
end

type t [@@deriving equal, sexp, compare, quickcheck]
(** Opaque type of types. *)

include
  Stringable.S with type t := t
(** Types can be converted to strings; this conversion is quite limited in
    both directions, mapping "basic*" for pointer types and "normal" for
    basic types only. *)

include
  Plumbing.Jsonable_types.S with type t := t
(** Types can be converted to JSON by stringification. *)

(** {2 Constructors} *)

val normal : Basic.t -> t
(** [normal ty] lifts a basic type [ty] to a scalar type. *)

val pointer_to : Basic.t -> t
(** [pointer_to ty] lifts a basic type [ty] to a pointer type. *)

val of_basic : Basic.t -> is_pointer:bool -> t
(** [of_basic ty ~is_pointer] lifts a basic type [ty] to a pointer type if
    [is_pointer] is true, and a normal one otherwise. *)

val bool_type : is_atomic:bool -> is_pointer:bool -> t
(** [bool_type ~is_atomic ~is_pointer] constructs the right Boolean type
    according to the flags [is_atomic] and [is_pointer]. *)

val int_type : is_atomic:bool -> is_pointer:bool -> t
(** [int_type ~is_atomic ~is_pointer] constructs the right integer type
    according to the flags [is_atomic] and [is_pointer]. *)

(** {2 Modifiers} *)

val to_non_atomic : t -> t Or_error.t
(** [to_non_atomic ty] tries to get the non-atomic type corresponding to the
    atomic type [ty]. It fails if [ty] isn't atomic. *)

val deref : t -> t Or_error.t
(** [deref ty] tries to strip a layer of pointer indirection off [ty]. It
    fails if [ty] isn't a pointer type. *)

val ref : t -> t Or_error.t
(** [ref ty] tries to add a layer of pointer indirection onto [ty]. It fails
    if [ty] is already a pointer type. *)

(** {2 Accessors and predicates} *)

val basic_type : t -> Basic.t
(** [basic_type ty] gets [ty]'s underlying basic type. *)

val basic_type_is : t -> basic:Basic.t -> bool
(** [basic_type_is ty ~basic] is true provided that [basic] is [ty]'s
    underlying basic type. *)

val is_atomic : t -> bool
(** [is_atomic ty] returns whether [ty] is an atomic type. *)

val is_pointer : t -> bool
(** [is_pointer ty] returns whether [ty] is a pointer type. *)
