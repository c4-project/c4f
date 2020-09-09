(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Low-level quickcheck generators for atomic fetches

    High-level atomic fetch generators depend on the main expression
    generator, and so appear in {!Expr}. *)

open Import

(** Type of atomic load generators. *)
module type S = sig
  type t = Fir.Expression.t Fir.Atomic_fetch.t
  [@@deriving sexp_of, quickcheck]
end

(** Generates random, type-safe atomic fetches using over the given variable
    typing environment to source objects, and the given operator and argument
    generators. *)
module Int
    (Obj : Fir.Env_types.S)
    (O : Utils.My_quickcheck.S_with_sexp with type t := Fir.Op.Fetch.t)
    (Arg : Utils.My_quickcheck.S_with_sexp with type t := Fir.Expression.t) :
  S
