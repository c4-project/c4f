(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

val gen :
     Fir.Constant.t
  -> Fir.Env.t
  -> int:(Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> bool:(Fir.Env.t -> Fir.Expression.t Q.Generator.t)
  -> int_load:
       (Fir.Env.t -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t)
  -> bool_load:
       (Fir.Env.t -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t)
  -> Fir.Expression.t Q.Generator.t
(** [gen k env ~int ~bool ~int_load ~bool_load] generates expressions over
    [env] that are statically guaranteed to result in the value [k]. It
    assumes that at least one bop rule that generates [k] exists. It uses
    [int] and [bool] to generate arbitrary expressions, and [int_load] and
    [bool_load] to generate loads, both over the known-value variables
    available in [env]. *)
