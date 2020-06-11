(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR expression generators *)

(** Type of expression generators. *)
module type S =
  Act_utils.My_quickcheck.S_with_sexp with type t = Expression.t

(** {1 Integers} *)

(** Generates random, type-safe expressions over the given variable
    environment, with type 'int'. *)
module Int_values (E : Env_types.S) : S

(** Generates random integer expressions that always evaluate to 0. *)
module Int_zeroes (E : Env_types.S) : S

(** {2 Atomic fetches}

    These depend recursively on the integer generators, and so can't be
    defined earlier than this. *)

module Atomic_fetch_int_nops (Obj : Env_types.S) (Arg : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Expression.t Atomic_fetch.t

module Atomic_fetch_int_values (Obj : Env_types.S) (Arg : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Expression.t Atomic_fetch.t

(** {1 Booleans} *)

(** Generates random, type-safe expressions over the given variable
    environment, with type 'bool'. *)
module Bool_values (E : Env_types.S) : S

(** {2 Booleans with known truth values}

    [Bool_known] constructs two generators over a given environment with
    known value data: a tautology generator and a falsehood generator. *)
module Bool_known (E : Env_types.S) : sig
  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'true' value. *)
  module Tautologies : S

  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'false' value. *)
  module Falsehoods : S
end

(** {2 Generators only} *)

val gen_bools : Env.t -> Expression.t Base_quickcheck.Generator.t
(** [gen_bools env] is shorthand for the [Bool_values] generator over [env]. *)

val gen_tautologies : Env.t -> Expression.t Base_quickcheck.Generator.t
(** [gen_tautologies env] is shorthand for the [Tautologies] generator of an
    instance of [Bool_known] over [env]. *)

val gen_falsehoods : Env.t -> Expression.t Base_quickcheck.Generator.t
(** [gen_falsehoods env] is shorthand for the [Falsehoods] generator of an
    instance of [Bool_known] over [env]. *)
