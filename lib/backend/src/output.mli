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

module Observation : sig
  type t [@@deriving sexp_of, quickcheck, yojson]
  (** Opaque type of a simulator observation record. *)

  (** {2 Constructing an observation record} *)

  val init : unit -> t
  (** [init ()] creates an initial observation record, with no states and no
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
end

type t =
  | Success of Observation.t
  | Skipped of {why: Info.t}
  | Errored of {err: Error.t}
[@@deriving sexp_of]

(** {2 Convenience constructors} *)

val join : t Or_error.t -> t

val of_observation_or_error : Observation.t Or_error.t -> t

val not_found : Fpath.t -> t

val to_observation_or_error :
  t -> handle_skipped:[`Error | `Ignore] -> Observation.t Or_error.t
