(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: type system

    This is a conservative subset of C11's type system, with a few things
    that'd normally be typedefs pulled into distinct types. *)

open Base
open Import

(** {1 Primitive types} *)
module Prim : sig
  type t = Bool | Int  (** Type of primitive types. *)

  val eq :
    (unit, t, 'x, [> Accessor.getter]) Accessor.t -> 'x -> to_:t -> bool
  (** [eq acc x ~to_] checks that the primitive type of [x], found via [acc],
      is equal to [to_]. *)
end

(** {1 Basic types}

    A basic type is a primitive type, or an atomic form of a primitive type. *)
module Basic : sig
  (** Opaque type of basic types. *)
  type t

  include Utils.Enum_types.Extension_table with type t := t

  (** {2 Constructors} *)

  val make : ?is_atomic:bool -> Prim.t -> t
  (** [make ?is_atomic p] lifts [p] to a basic type of atomicity [is_atomic]. *)

  val bool : ?is_atomic:bool -> unit -> t
  (** [bool ?is_atomic ()] is the (C99?) Boolean, or C11 atomic_bool, type. *)

  val int : ?is_atomic:bool -> unit -> t
  (** [int] is the int, or C11 atomic_int, type. *)

  (** {2 Accessors} *)

  module Access : sig
    val prim : (_, Prim.t, t, [< Accessor.field]) Accessor.t
    (** [prim] accesses the primitive type of a basic type. *)

    val is_atomic : (_, bool, t, [< Accessor.field]) Accessor.t
    (** [is_atomic] accesses the atomicity of a basic type. *)
  end

  val eq :
    (unit, t, 'x, [> Accessor.getter]) Accessor.t -> 'x -> to_:t -> bool
  (** [eq acc x ~to_] checks that the primitive type of [x], found via [acc],
      is equal to [to_]. *)

  (** {2 Getters} *)

  val prim : t -> Prim.t
  (** [prim btype] gets the primitive type of [btype]. *)

  val is_atomic : t -> bool
  (** [is_atomic btype] is true if the basic type [btype] is atomic. *)

  val to_non_atomic : t -> t Or_error.t
  (** [to_non_atomic btype] tries to get the non-atomic type corresponding to
      the atomic type [btype]. It fails if [btype] isn't atomic. *)

  val as_atomic : t -> t
  (** [as_atomic btype] returns [btype] if it is atomic, or the atomic
      equivalent of [btype] if not. *)

  val strip_atomic : t -> t
  (** [strip_atomic btype] behaves as [to_non_atomic btype] if [btype] is
      atomic, and returns [btype] if not. *)
end

(** Opaque type of types. *)
type t [@@deriving equal, sexp, compare, quickcheck]

(** Types can be converted to strings; this conversion is quite limited in
    both directions, mapping "basic*" for pointer types and "normal" for
    basic types only. *)
include Stringable.S with type t := t

(** Types can be converted to JSON by stringification. *)
include Plumbing.Jsonable_types.S with type t := t

(** {2 Constructors} *)

val make : ?is_pointer:bool -> ?is_volatile:bool -> Basic.t -> t
(** [make ?is_pointer ?is_volatile ty] makes a type from a basic type [ty],
    applying any flags that are present and set to [true]. *)

val bool :
  ?is_atomic:bool -> ?is_pointer:bool -> ?is_volatile:bool -> unit -> t
(** [bool ?is_atomic ?is_pointer ()] constructs the right Boolean type
    according to the flags [is_atomic], [is_pointer], and [is_volatile], all
    of which default to [false]. *)

val int :
  ?is_atomic:bool -> ?is_pointer:bool -> ?is_volatile:bool -> unit -> t
(** [int ?is_atomic ?is_pointer ()] constructs the right integer type
    according to the flags [is_atomic], [is_pointer], and [is_volatile], all
    of which default to [false]. *)

(** {2 Accessors} *)

module Access : sig
  val basic_type : (_, Basic.t, t, [< Accessor.field]) Accessor.t
  (** [basic_type] accesses the basic type of a type. *)

  val is_pointer : (_, bool, t, [< Accessor.field]) Accessor.t
  (** [is_pointer] accesses the pointerness of a type. *)

  val is_volatile : (_, bool, t, [< Accessor.field]) Accessor.t
  (** [is_volatile] accesses the volatility of a type. *)
end

(** {2 Modifiers} *)

(** {3 Pointers} *)

val deref : t -> t Or_error.t
(** [deref ty] tries to strip a layer of pointer indirection off [ty]. It
    fails if [ty] isn't a pointer type. *)

val ref : t -> t Or_error.t
(** [ref ty] tries to add a layer of pointer indirection onto [ty]. It fails
    if [ty] is already a pointer type. *)

(** {3 Atomicity} *)

val to_non_atomic : t -> t Or_error.t
(** [to_non_atomic ty] tries to get the non-atomic type corresponding to the
    atomic type [ty]. It fails if [ty] isn't atomic. *)

val as_atomic : t -> t
(** [as_atomic ty] returns [ty] if it is atomic, or the atomic equivalent of
    [ty] if not. *)

val strip_atomic : t -> t
(** [strip_atomic ty] behaves as [to_non_atomic ty] if [ty] is atomic, and
    returns [ty] if not. *)

(** {3 Volatility} *)

val as_volatile : t -> t
(** [as_volatile ty] returns [ty] if it is volatile, or the volatile
    equivalent of [ty] if not. *)

(** {2 Getters and predicates} *)

val basic_type : t -> Basic.t
(** [basic_type ty] gets [ty]'s underlying basic type. *)

val basic_type_is : t -> basic:Basic.t -> bool
(** [basic_type_is ty ~basic] is true provided that [basic] is [ty]'s
    underlying basic type. *)

val is_atomic : t -> bool
(** [is_atomic ty] returns whether [ty] is an atomic type. *)

val is_pointer : t -> bool
(** [is_pointer ty] returns whether [ty] is a pointer type. *)

val is_volatile : t -> bool
(** [is_volatile ty] returns whether [ty] is a volatile type. *)

(** {2 Type checker building blocks} *)

val check : t -> t -> t Or_error.t
(** [check t1 t2] raises an error if [t1] is not equal to [t2]. It returns
    [t1] on success. *)

val check_modulo_atomicity : t -> t -> t Or_error.t
(** [check_modulo_atomicity t1 t2] raises an error if [t1] is not equal to
    [t2], modulo [t1] and [t2]'s atomicity. It returns [strip_atomic t1] on
    success. *)

val check_atomic_non : atomic:t -> non:t -> t Or_error.t
(** [check_atomic_non ~atomic ~non] checks that [atomic] is an atomic type,
    and its non-atomic equivalent is equal to [non]. It returns the
    non-atomic form of [atomic] on success. *)

val check_pointer_non : pointer:t -> non:t -> t Or_error.t
(** [check_pointer_non ~atomic ~non] checks that [pointer] is a pointer type,
    and its dereferenced equivalent is equal to [non]. It returns the
    deferenced form of [pointer] on success. *)
