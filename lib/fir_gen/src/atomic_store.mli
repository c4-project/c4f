(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quickcheck generators for typesafe atomic stores *)

open Import

(** Type of atomic store generators. *)
module type S = sig
  type t = Fir.Atomic_store.t [@@deriving sexp_of, quickcheck]
end

(** Generates type-safe, but arbitrary, integer atomic stores over the given
    environments. *)
module Int (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S

(** Generates type-safe, but arbitrary, Boolean atomic stores over the given
    environments. *)
module Bool (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S
