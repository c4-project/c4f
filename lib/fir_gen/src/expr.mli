(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR expression generators *)

open Import

(** Type of expression generators. *)
module type S =
  Utils.My_quickcheck.S_with_sexp with type t = Fir.Expression.t

(** {1 Integers} *)

(** Generates random, type-safe expressions over the given variable
    environment, with type 'int'. *)
module Int_values (E : Fir.Env_types.S) : S

(** Generates random integer expressions that always evaluate to 0. *)
module Int_zeroes (E : Fir.Env_types.S) : S

(** {2 Atomic fetches}

    These depend recursively on the integer generators, and so can't be
    defined earlier than this. *)

module Atomic_fetch_int_nops (Obj : Fir.Env_types.S) (Arg : Fir.Env_types.S) :
  Utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t

module Atomic_fetch_int_values
    (Obj : Fir.Env_types.S)
    (Arg : Fir.Env_types.S) :
  Utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t

(** {1 Booleans} *)

(** Generates random, type-safe expressions over the given variable
    environment, with type 'bool'. *)
module Bool_values (E : Fir.Env_types.S) : S

(** {2 Booleans with known truth values}

    [Bool_known] constructs two generators over a given environment with
    known value data: a tautology generator and a falsehood generator. *)
module Bool_known (E : Fir.Env_types.S) : sig
  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'true' value. *)
  module Tautologies : S

  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'false' value. *)
  module Falsehoods : S
end

(** {2 Generators only} *)

val bool : Fir.Env.t -> Fir.Expression.t Q.Generator.t
(** [bool env] generates arbitrary Boolean expressions over [env]. *)

val int : Fir.Env.t -> Fir.Expression.t Q.Generator.t
(** [int env] generates arbitrary integer expressions over [env]. *)

val tautology : Fir.Env.t -> Fir.Expression.t Q.Generator.t
(** [tautology env] is shorthand for the [Tautologies] generator of an
    instance of [Bool_known] over [env]. *)

val falsehood : Fir.Env.t -> Fir.Expression.t Q.Generator.t
(** [falsehood env] is shorthand for the [Falsehoods] generator of an
    instance of [Bool_known] over [env]. *)
