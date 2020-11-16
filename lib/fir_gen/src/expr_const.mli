(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

(** [gen k env ~gen_arb ~gen_load] generates expressions over [env] that are
    statically guaranteed to result in the value [k].  It assumes that at least
    one bop rule that generates [k] exists.  It uses [gen_arb] to generate
    arbitrary expressions, and [gen_load] to generate loads, both over
    the known-value variables available in [env]. *)
val gen : Fir.Constant.t -> Fir.Env.t -> gen_arb:(Fir.Env.t -> Fir.Expression.t Q.Generator.t) -> gen_load:(Fir.Env.t -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t) -> Fir.Expression.t Q.Generator.t