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

(* Don't use Base: it causes us to derive a weaker signature *)
open Core_kernel
open Utils

(** Litmus-style identifiers.

    These identifiers extend C-style identifiers with an optional
    thread identifier, and capture the type of identifier seen in
    Litmus postconditions. *)

(** Opaque type of litmus-style identifiers. *)
type t
[@@deriving compare, sexp, quickcheck]

(** {2 Constructors} *)

(** [global id] creates a global identifier. *)
val global : C_identifier.t -> t

(** [global_of_string str] tries to create a global identifier from [str].
    It fails if [str] isn't a valid C identifier. *)
val global_of_string : string -> t Or_error.t

(** [local tid id] creates a local identifier with the given thread ID. *)
val local : int -> C_identifier.t -> t

(** [try_parse str] tries to parse [str] as a Litmus identifier. *)
val try_parse : string -> t Or_error.t

(** {2 Accessors} *)

(** [as_global id] gets [Some cid] if [id] is the global identifier [cid],
    or [None] otherwise. *)
val as_global : t -> C_identifier.t option

(** [tid id] gets [id]'s thread identifier, if it has one. *)
val tid : t -> int option

(** [to_memalloy_id id] converts [id] to the corresponding
    memalloy executable-C global variable name.

    This is [x] where [id = Global x], and ["tXY"] where
    [id = Local (X, Y)]. *)
val to_memalloy_id : t -> C_identifier.t

(** {2 Interface implementations} *)

(** Litmus identifiers can be converted to and from strings.
    Note that conversion from strings can fail if the C identifier
    parts don't obey C identifier validation. *)
include Stringable.S with type t := t

(** Litmus identifiers can be pretty-printed. *)
include Pretty_printer.S with type t := t

(** Litmus identifiers suit various comparable scenarios, such as
    map keys. *)
include Comparable.S with type t := t
