(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Expression generator stubs for arbitrary Boolean expressions.

    These generators expect an arbitrary integer generator.

    For fully-formed, recursive generators, see {!Expr}. For known-constant
    generators, see {!Expr_const}. *)

open Import

(** We don't have a [gen_loadlike] for Booleans, but this may change. *)

val gen :
     Fir.Env.t
  -> int:(Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> Fir.Expression.t Q.Generator.t
(** [gen env ~int] generates arbitrary, type-safe, non-side-effectful Boolean
    expressions over [env]. It uses [int] to generate arbitrary integer
    expressions. *)
