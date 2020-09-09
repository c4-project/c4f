(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quickcheck generators for typesafe atomic stores *)

open Import

(** Type of assign generators. *)
module type S = sig
  type t = Fir.Assign.t [@@deriving sexp_of, quickcheck]
end

(** Generates type-safe, but arbitrary, integer assigns over the given environments. *)
module Int (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S

(** Generates type-safe, but arbitrary, Boolean assigns over the given environments. *)
module Bool (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S