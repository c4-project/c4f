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

(** Fuzzer: high-level actions *)

open Core_kernel

(** Enumeration of kinds of action. *)
module Kind : sig
  type t =
    | Make_global
    | Make_constant_store
  ;;
end

module Table : sig
  (** Type of weightings in an action table. *)
  module Weight : sig
    type t
    (** Opaque type of weights. *)

    val create : int -> t Or_error.t
    (** [create k] tries to build a weight from an integer [k]. *)
  end

  module Row : sig
    type 'a t =
      { kind      : Kind.t
      (** The kind of action this row represents. *)
      ; weight    : Weight.t
      (** The weight given to this action. *)
      ; predicate : 'a -> bool
      (** The predicate that must hold to consider this action. *)
      }
    ;;
  end

  type 'a t = 'a Row.t list
  (** Type of action tables. *)
end

(** Fully-generated action payloads. *)
module Payload : sig
  type t =
    | Make_global of { is_atomic : bool; initial_value : int }
    | Make_constant_store of { new_value : int }
  ;;

  val gen : t Quickcheck.Generator.t
  (** Generates a random [Payload]. *)
end
