(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: type system

    This is a conservative subset of C11's type system, with a few things
    that'd normally be typedefs pulled into distinct types. *)

open Base
open Act_utils

(** Primitive types. *)
module Basic : sig
  (** Opaque type of basic types. *)
  type t

  val bool : ?atomic:bool -> unit -> t
  (** [bool ?atomic ()] is the (C99?) Boolean, or C11 atomic_bool, type. *)

  val int : ?atomic:bool -> unit -> t
  (** [int] is the int, or C11 atomic_int, type. *)

  include Enum_types.Extension_table with type t := t

  val to_spec : t -> [> Act_c_lang.Ast.Type_spec.t]
  (** [to_spec btype] converts a basic type to a type spec. *)

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

val normal : Basic.t -> t
(** [normal ty] lifts a basic type [ty] to a scalar type. *)

val pointer_to : Basic.t -> t
(** [pointer_to ty] lifts a basic type [ty] to a pointer type. *)

val of_basic : ?pointer:bool -> Basic.t -> t
(** [of_basic ?is_pointer ty] lifts a basic type [ty] to a pointer type if
    [is_pointer] is true, and a normal one otherwise (and by default). *)

val bool : ?atomic:bool -> ?pointer:bool -> unit -> t
(** [bool ?atomic ?pointer ()] constructs the right Boolean type according to
    the flags [atomic] and [pointer], both of which default to [false]. *)

val int : ?atomic:bool -> ?pointer:bool -> unit -> t
(** [int ?atomic ?pointer ()] constructs the right integer type according to
    the flags [atomic] and [pointer], both of which default to [false]. *)

(** {2 Modifiers} *)

val to_non_atomic : t -> t Or_error.t
(** [to_non_atomic ty] tries to get the non-atomic type corresponding to the
    atomic type [ty]. It fails if [ty] isn't atomic. *)

val as_atomic : t -> t
(** [as_atomic ty] returns [ty] if it is atomic, or the atomic equivalent of
    [ty] if not. *)

val strip_atomic : t -> t
(** [strip_atomic ty] behaves as [to_non_atomic ty] if [ty] is atomic, and
    returns [ty] if not. *)

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
