(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Atomic_store.t [@@deriving sexp_of, quickcheck]
end

module Int (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Atomic_store.Quickcheck_generic
    (Expr.Int_values (Src)) (Address.Atomic_int_pointers (Dst))

module Bool (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Atomic_store.Quickcheck_generic
    (Expr.Bool_values (Src)) (Address.Atomic_bool_pointers (Dst))
