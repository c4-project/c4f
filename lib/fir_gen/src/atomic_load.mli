(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quickcheck generators for atomic loads. *)

open Import

(** Type of atomic load generators. *)
module type S = sig
  type t = Fir.Atomic_load.t [@@deriving sexp_of, quickcheck]
end

(** Generates random, type-safe atomic loads over the given variable typing
    environment, restricted to atomic bools. *)
module Bool (E : Fir.Env_types.S) : S

(** Generates random, type-safe atomic loads over the given variable typing
    environment, restricted to atomic ints. *)
module Int (E : Fir.Env_types.S) : S
