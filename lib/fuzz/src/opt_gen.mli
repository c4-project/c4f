(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of generators that can fail to yield at construction time.

    The ACT fuzzer uses Jane Street's Quickcheck infrastructure to generate
    various pseudorandom elements (paths, payloads, payloads containing
    paths, etc). Many of these generators depend on the existence of
    particular patterns in the current fuzzer state to run, and their failure
    to yield is not necessarily an error.

    This module captures the idea of a partial Quickcheck generator as an
    applicative functor. (It isn't a monad, as there is no way to pull the
    partiality of a generator outside of the inner Quickcheck monad.) It also
    provides helper functions for constructing partial generators. *)

open Base

(** The type of partial generators. *)
type 'a t = 'a Base_quickcheck.Generator.t Or_error.t

(** Partial generators are an applicative functor. *)
include Applicative.S with type 'a t := 'a t

(** {1 Helpers} *)

val union : 'a t list -> 'a t
(** [union xs] combines a list of partial generators into a partial
    generator. If there are no valid generators, the result is an error;
    else, it is the union of the viable generators in [xs]. *)
