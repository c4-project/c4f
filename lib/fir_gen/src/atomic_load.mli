(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quickcheck generators for atomic loads. *)

open Import

(** Type of atomic load generators. *)
module type S = sig
  type t = Fir.Atomic_load.t [@@deriving sexp_of, quickcheck]
end

(** Generates random, type-safe atomic loads over the given variable typing
    environment, restricted to atomic ints. *)
module Int (E : Fir.Env_types.S) : S
