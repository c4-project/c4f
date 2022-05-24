(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
