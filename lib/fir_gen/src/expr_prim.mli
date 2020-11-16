(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Generators for primitive expressions.

    These are loads (atomic and non-atomic) and constants.

    There is no FIR-level distinction between expressions and primitive
    expressions yet, but this may change (making a distinction between atomic
    loads and atomic RMWs, perhaps). *)

open Import

(** {1 Generating integer primitives} *)
module Int : sig
  val gen_load :
    Fir.Env.t -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t
  (** [gen_load env] generates atomic and/or non-atomic integer loads over
      [env], together with their variable records. *)

  val gen : Fir.Env.t -> Fir.Expression.t Q.Generator.t
  (** [gen env] generates integer primitives over [env]. *)
end

(** {1 Generating Boolean primitives} *)
module Bool : sig
  val gen_load :
    Fir.Env.t -> (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t
  (** [gen_load env] generates atomic and/or non-atomic Boolean loads over
      [env], together with their variable records. *)

  val gen : Fir.Env.t -> Fir.Expression.t Q.Generator.t
  (** [gen env] generates Boolean primitives over [env]. *)
end
