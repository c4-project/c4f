(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C expression generators *)

(** Type of expression generators. *)
module type S =
  Act_utils.My_quickcheck.S_with_sexp with type t = Expression.t

(** Generates random, type-safe expressions over the given variable
    environment, with type 'int'. *)
module Int_values (E : Env_types.S_with_known_values) : S

(** Generates random integer expressions that always evaluate to 0. *)
module Int_zeroes (Env : Env_types.S_with_known_values) : S

(** {1 Booleans} *)

(** Generates random, type-safe expressions over the given variable
    environment, with type 'bool'. *)
module Bool_values (E : Env_types.S_with_known_values) : S

(** {2 Booleans with known truth values}

    [Bool_known] constructs two generators over a given environment with
    known value data: a tautology generator and a falsehood generator. *)
module Bool_known (E : Env_types.S_with_known_values) : sig
  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'true' value. *)
  module Tautologies : S

  (** Generates random, type-safe expressions over the given variable
      environment, with type 'bool' and guaranteed 'false' value. *)
  module Falsehoods : S
end
