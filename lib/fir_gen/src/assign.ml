(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Assign.t [@@deriving sexp_of, quickcheck]
end

module Int (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Assign.Quickcheck_generic
    (Expr.Int_values (Src)) (Lvalue.Int_values (Dst))

module Bool (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Assign.Quickcheck_generic
    (Expr.Bool_values (Src)) (Lvalue.Bool_values (Dst))
