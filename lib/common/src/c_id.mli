(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Strings that obey C identifier restrictions.

    Despite the name, this module isn't just for use in C ASTs: it also sees
    use in Litmus identifiers, etc. *)

(* The actual implementation depends on Core_kernel, for now, but we only
   expose the part of it that adheres to the Base interfaces. *)
open Base

type t [@@deriving compare, hash, sexp, quickcheck]
(** Opaque type of C identifier strings. *)

val create : string -> t Or_error.t
(** [create str] creates a C identifier string from [str]. It returns an
    error if [str] isn't a valid C identifier. *)

val create_exn : string -> t
(** [create_exn str] creates a C identifier string from [str]. It raises an
    exception if [str] isn't a valid C identifier. *)

include Comparable.S with type t := t

include Pretty_printer.S with type t := t

include Plumbing.Jsonable_types.S with type t := t

include
  Stringable.S with type t := t
(** Note that [of_string] is [create_exn]; ie, it can fail. *)

val is_string_safe : string -> bool
(** [is_string_safe str] checks whether [str] is C-safe, but doesn't return
    the constructed identifier. *)

(** A restricted form of C identifiers that represents identifiers that Herd
    can safely lex. *)
module Herd_safe : sig
  type c = t

  type t [@@deriving bin_io, compare, hash, sexp, quickcheck]

  val create : string -> t Or_error.t
  (** [create str] creates a Herd-safe C identifier string from [str]. It
      returns an error if [str] isn't a Herd-safe C identifier. *)

  include Pretty_printer.S with type t := t

  include
    Stringable.S with type t := t
  (** Note that [of_string] is [create_exn]; ie, it can fail. *)

  val of_c_identifier : c -> t Or_error.t
  (** [of_c_identifier cid] tries to create a Herd-safe identifier from
      [cid]. It returns an error if [str] isn't a Herd-safe C identifier. *)

  val to_c_identifier : t -> c
  (** [to_c_identifier cid] converts a Herd-safe identifier to a normal one. *)

  val is_string_safe : string -> bool
  (** [is_string_safe str] checks whether [str] is Herd-safe, but doesn't
      return the constructed identifier. *)
end

(** Associative lists with C identifier keys, with derived operations. *)
module Alist : sig
  include
    Travesty.Bi_traversable_types.S1_right
      with type 'r t = (t, 'r) List.Assoc.t
       and type left := t

  val yojson_of_t : ('r -> Yojson.Safe.t) -> 'r t -> Yojson.Safe.t
  (** [to_yojson rhs assoc] serialises [assoc] to a JSON object, using [rhs]
      as the serialiser for values. *)

  val t_of_yojson : (Yojson.Safe.t -> 'r) -> Yojson.Safe.t -> 'r t

  val t_of_yojson' :
       (Yojson.Safe.t -> ('r, string) Result.t)
    -> Yojson.Safe.t
    -> ('r t, string) Result.t
end
