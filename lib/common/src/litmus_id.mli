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

(* Don't use Base: it causes us to derive a weaker signature *)
open Core_kernel

(** Litmus-style identifiers.

    These identifiers extend C-style identifiers with an optional thread
    identifier, and capture the type of identifier seen in Litmus
    postconditions. *)

(** Opaque type of litmus-style identifiers. *)
type t [@@deriving compare, sexp, quickcheck]

(** {2 Constructors} *)

val global : C_id.t -> t
(** [global id] creates a global identifier. *)

val global_of_string : string -> t Or_error.t
(** [global_of_string str] tries to create a global identifier from [str].
    It fails if [str] isn't a valid C identifier. *)

val local : int -> C_id.t -> t
(** [local tid id] creates a local identifier with the given thread ID. *)

val try_parse : string -> t Or_error.t
(** [try_parse str] tries to parse [str] as a Litmus identifier. *)

(** {2 Accessors} *)

val as_global : t -> C_id.t option
(** [as_global id] gets [Some cid] if [id] is the global identifier [cid],
    or [None] otherwise. *)

val as_local : t -> (int * C_id.t) option
(** [as_local id] gets [Some (tid, cid)] if [id] is the local identifier [cid]
    in thread [tid], or [None] otherwise. *)

val variable_name : t -> C_id.t
(** [variable_name id] gets the underlying variable name of [id]. *)

val tid : t -> int option
(** [tid id] gets [id]'s thread identifier, if it has one. *)

val to_memalloy_id : t -> C_id.t
(** [to_memalloy_id id] converts [id] to the corresponding memalloy
    executable-C global variable name.

    This is [x] where [id = Global x], and ["tXY"] where
    [id = Local (X, Y)]. *)

(** {2 Interface implementations} *)

(** Litmus identifiers can be converted to and from strings. Note that
    conversion from strings can fail if the C identifier parts don't obey C
    identifier validation. *)
include Stringable.S with type t := t

(** Litmus identifiers can be pretty-printed. *)
include Pretty_printer.S with type t := t

(** Litmus identifiers suit various comparable scenarios, such as map keys. *)
include Comparable.S with type t := t

(** Monadic traversal over the C identifier part of a Litmus identifier. *)
module On_c_identifiers : Travesty.Traversable.S0
  with type t = t
   and type Elt.t = C_id.t

(** Helpers for parsing, and handling, associative lists over litmus IDs. *)
module Assoc : sig
  type nonrec 'a t = (t, 'a) List.Assoc.t

  val try_parse :
       string list
    -> value_parser:(string option -> 'a Or_error.t)
    -> 'a t Or_error.t
  (** [try_parse strs ~value_parser] tries to parse the string list [strs],
      where each string is of the form [id=value], as an associative list
      with litmus ID keys and values interpreted by [value_parser]. *)
end
