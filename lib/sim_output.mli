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

(** Abstract data types for simulation output. *)

open Base
open Utils

(** A single state in a Herd/Litmus run output. *)
module State : sig
  (** [t] is the type of states: a binding from name to value. *)
  type t [@@deriving sexp, compare, quickcheck]

  val map :
       location_map:(Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    -> value_map:(string -> string Or_error.t)
    -> t
    -> t Or_error.t
  (** [map ~location_map ~value_map t] maps partial mappers over the keys
      and values of state [t]. [location_map] may return [Ok None] if the
      key should be deleted in the new map.

      If all invocations of [location_map] and [value_map] return values,
      and the result is a well-formed map [m], [map] returns [Ok m]; else,
      an error. *)

  val of_alist : (Litmus.Id.t, string) List.Assoc.t -> t Or_error.t
  (** [of_alist alist] tries to convert [alist] into a state. *)

  module Set : My_set.S with type Elt.t = t

  val bound : t -> Litmus.Id.t list
  (** [bound t] gets the list of all bound names in [t]. *)

  val restrict : t -> domain:Litmus.Id.Set.t -> t
  (** [restrict t ~domain] removes all mappings in [t] that don't reference
      identifiers in [domain]. *)
end

(** Opaque type of a simulator output record. *)
type t [@@deriving sexp_of, quickcheck]

(** {2 Constructing an output record} *)

val init : unit -> t
(** [init ()] creates an initial output record, with no states and no
    undefined behaviour flag. *)

val add : t -> state:State.t -> t Or_error.t
(** [add out ~state] adds [state] onto the state set of [out]. It fails if
    [out] is marked as having undefined behaviour. *)

val set_undefined : t -> t Or_error.t
(** [set_undefined out] marks [out] with an undefined behaviour flag. It
    fails if [out] is already marked as having undefined behaviour, or has
    states attached. *)

(** {2 Accessors} *)

val states : t -> State.t list
(** [states out] gets the states of a single simulator output [out]. *)

val is_undefined : t -> bool
(** [is_undefined out] gets whether [out]'s run encountered undefined
    behaviour. *)
