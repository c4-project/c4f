(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Low-level quickcheck generators for atomic fetches

    High-level atomic fetch generators depend on the main expression
    generator, and so appear in {!Expr}. *)

open Import

type env := Fir.Env.t

type t := Fir.Expression.t Fir.Atomic_fetch.t

type const_f := Fir.Constant.t -> Fir.Expression.t Q.Generator.t

(** Type of atomic load generators. *)
module type S = sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end

(** Generates random, type-safe atomic fetches using over the given variable
    typing environment to source objects, and the given operator and argument
    generators. *)
module Int
    (_ : Fir.Env_types.S)
    (_ : Utils.My_quickcheck.S_with_sexp with type t := Fir.Op.Fetch.t)
    (_ : Utils.My_quickcheck.S_with_sexp with type t := Fir.Expression.t) :
  S

(** {2 Specific integer generators} *)

val gen_zero_idem : env -> const:const_f -> t Q.Generator.t
(** [gen_zero_idem env ~const] generates atomic fetches over the variables in
    [env] such that the argument to the fetch is an expression generated with
    [const] evaluating to zero, and the fetch is idempotent. *)

val gen_neg1_idem : env -> const:const_f -> t Q.Generator.t
(** [gen_neg1_idem env ~const] generates atomic fetches over the variables in
    [env] such that the argument to the fetch is an expression generated with
    [const] evaluating to negative 1, and the fetch is idempotent. *)

val gen_int_refl_idem : env -> t Q.Generator.t
(** [gen_int_refl_idem env] generates atomic fetches over the variables in
    [env] that have known values, such that the argument to the fetch is the
    known value of the variable and the fetch is idempotent. *)

val gen_int_idem : env -> const:const_f -> t Q.Generator.t
(** [gen_int_idem env ~const] generates idempotent integer atomic fetches
    over the variables in [env], perhaps using [const] to generate constants. *)
