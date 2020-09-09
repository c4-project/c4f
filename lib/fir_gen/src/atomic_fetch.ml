(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Expression.t Fir.Atomic_fetch.t
  [@@deriving sexp_of, quickcheck]
end

module Int
    (Obj : Fir.Env_types.S)
    (O : Utils.My_quickcheck.S_with_sexp with type t := Fir.Op.Fetch.t)
    (Arg : Utils.My_quickcheck.S_with_sexp with type t := Fir.Expression.t) :
  S =
  Fir.Atomic_fetch.Quickcheck_generic
    (Address.Atomic_int_pointers (Obj)) (O)
    (struct
      type t = Fir.Expression.t

      include Arg
    end)
