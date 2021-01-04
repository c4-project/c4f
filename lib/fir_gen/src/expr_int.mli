(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Expression generator stubs for arbitrary integer expressions.

    These generators expect an integer constant generator, which, in turn,
    will usually recursively expect an arbitrary integer generator.

    For fully-formed, recursive generators, see {!Expr}. For known-constant
    generators, see {!Expr_const}. *)

open Import

val gen_loadlike :
     Fir.Env.t
  -> const:(Fir.Constant.t -> Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t
(** [gen_loadlike env ~const] generates loads and load-like expressions (eg
    no-op RMWs using [const]) over [env]. Each generated expression comes
    with the record of the loaded variable. *)

val gen :
     Fir.Env.t
  -> bool:(Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> const:(Fir.Constant.t -> Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> Fir.Expression.t Q.Generator.t
(** [gen env ~bool ~const] generates arbitrary, type-safe, non-side-effectful
    integer expressions over [env], using [int_const] to generate expressions
    with particular known-constant values. *)
