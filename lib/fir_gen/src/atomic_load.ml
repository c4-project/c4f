(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Atomic_load.t [@@deriving sexp_of, quickcheck]
end

module Bool (E : Fir.Env_types.S) : S =
  Fir.Atomic_load.Quickcheck_generic (Address.Atomic_bool_pointers (E))

module Int (E : Fir.Env_types.S) : S =
  Fir.Atomic_load.Quickcheck_generic (Address.Atomic_int_pointers (E))
