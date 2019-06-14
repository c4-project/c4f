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

(** Strings that obey C identifier restrictions.

    Despite the name, this module isn't just for use in C ASTs: it also sees
    use in Litmus identifiers, etc. *)

open Core_kernel

(** Opaque type of C identifier strings. *)
type t [@@deriving bin_io, compare, hash, sexp, quickcheck]

val create : string -> t Or_error.t
(** [create str] creates a C identifier string from [str]. It returns an
    error if [str] isn't a valid C identifier. *)

val create_exn : string -> t
(** [create_exn str] creates a C identifier string from [str]. It raises an
    exception if [str] isn't a valid C identifier. *)

include Comparable.S with type t := t

include Pretty_printer.S with type t := t

include Plumbing.Loadable_types.Jsonable with type t := t

(** Note that [of_string] is [create_exn]; ie, it can fail. *)
include Stringable.S with type t := t

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

  (** Note that [of_string] is [create_exn]; ie, it can fail. *)
  include Stringable.S with type t := t

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
    Travesty.Bi_mappable.S1_right
    with type 'r t = (t, 'r) List.Assoc.t
     and type left := t

  val to_yojson : ('r -> Yojson.Safe.t) -> 'r t -> Yojson.Safe.t
  (** [to_yojson rhs assoc] serialises [assoc] to a JSON object, using [rhs]
      as the serialiser for values. *)

  val of_yojson_exn : (Yojson.Safe.t -> 'r) -> Yojson.Safe.t -> 'r t

  val of_yojson : (Yojson.Safe.t -> ('r, string) Result.t) -> Yojson.Safe.t -> ('r t, string) Result.t
end
