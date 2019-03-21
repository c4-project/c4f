(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Parsing and comparison functionality for Herd output

    [Herd_output] contains functions for scraping the human-readable summary of a
    Herd7 run, extracting location information, and comparing states.
*)

open Core_kernel
open Utils

(** [t] is the opaque type of a Herd output analysis. *)
type t

module State : sig
  (** [t] is the type of states: a binding from name to value. *)
  type t [@@deriving sexp, compare]

  (** [map ~keyf ~valf t] maps partial mappers over the keys and
      values of state [t].  [keyf] may return [Ok None] if the
      key should be deleted in the new map.

      If all invocations of [keyf] and
      [valf] return values, and the result is a well-formed map
      [m], [map] returns [Ok m]; else, an error. *)
  val map
    :  keyf:(Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    -> valf:(string -> string Or_error.t)
    -> t
    -> t Or_error.t
  ;;

  (** [bound t] gets the list of all bound names in [t]. *)
  val bound : t -> Litmus.Id.t list

  (** [of_alist alist] tries to convert [alist] into a state. *)
  val of_alist
    :  (Litmus.Id.t, string) List.Assoc.t
    -> t Or_error.t
  ;;

  module Set : Set.S with type Elt.t = t
end

(** [single_outcome] is the type of outcomes we can get from single
    (or double) Herd runs. *)
type single_outcome =
  [ `Unknown   (** Either only one Herd run was analysed, or the
                   results are inconclusive. *)
  | `Undef     (** The final execution triggered undefined
                   behaviour. *)
  ]
[@@deriving sexp]
;;

(** [outcome] is the type of summaries of Herd analysis. *)
type outcome =
  [ single_outcome
  | `Order of State.Set.t My_set.Partial_order.t
  | `OracleUndef  (** The oracle execution triggered undefined
                      behaviour. *)
  ]
[@@deriving sexp] (* sexp_of_outcome, outcome_of_sexp *)
;;

(** [states herd] gets the states of a single Herd output [herd]. *)
val states : t -> State.t list

(** [single_outcome_of herd] analyses the Herd output [herd] in
    isolation. *)
val single_outcome_of : t -> single_outcome

(** [outcome ~initial ~final ~locmap ~valmap] applies the partial
    mappers [locmap] and [valmap] to every state binding in
    [final], then analyses it against [initial]. *)
val outcome_of
  :  initial : t
  -> final   : t
  -> locmap  : (Litmus.Id.t -> Litmus.Id.t option Or_error.t)
  -> valmap  : (string -> string Or_error.t)
  -> outcome Or_error.t
;;

(** We can load Herd output analyses using the normal interfaces in
    [Loadable]. *)
include Loadable.S with type t := t
