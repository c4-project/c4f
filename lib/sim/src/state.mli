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

(** A single state in a simulator run output. *)

open Base
open Act_common

(** [t] is the type of states: a binding from name to value. *)
type t [@@deriving sexp, compare, quickcheck]

include Comparable.S with type t := t

val map :
     location_map:(Litmus_id.t -> Litmus_id.t option Or_error.t)
  -> value_map:(string -> string Or_error.t)
  -> t
  -> t Or_error.t
(** [map ~location_map ~value_map t] maps partial mappers over the keys and
    values of state [t]. [location_map] may return [Ok None] if the key
    should be deleted in the new map.

    If all invocations of [location_map] and [value_map] return values, and
    the result is a well-formed map [m], [map] returns [Ok m]; else, an
    error. *)

val of_alist : (Litmus_id.t, string) List.Assoc.t -> t Or_error.t
(** [of_alist alist] tries to convert [alist] into a state. *)

val bound : t -> Litmus_id.t list
(** [bound t] gets the list of all bound names in [t]. *)

val restrict : t -> domain:Set.M(Litmus_id).t -> t
(** [restrict t ~domain] removes all mappings in [t] that don't reference
    identifiers in [domain]. *)
