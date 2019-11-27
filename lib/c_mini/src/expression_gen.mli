(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C expression generators *)

module type S =
  Act_utils.My_quickcheck.S_with_sexp with type t = Expression.t
(** Type of expression generators. *)

(** Generates random, type-safe expressions over the given variable typing
    environment, with type 'int'. *)
module Int_values (E : Env_types.S) : S

(** {1 Booleans} *)

(** Generates random, type-safe expressions over the given variable typing
    environment, with type 'bool'. *)
module Bool_values (E : Env_types.S) : S

(** {2 Booleans with known truth values}

    [Bool_known] constructs two generators over a given environment with
    known value data: a tautology generator and a falsehood generator. *)
module Bool_known (E : Env_types.S_with_known_values) : sig
  module Tautologies : S
  (** Generates random, type-safe expressions over the given variable typing
      environment, with type 'bool' and guaranteed 'true' value. *)

  module Falsehoods : S
  (** Generates random, type-safe expressions over the given variable typing
      environment, with type 'bool' and guaranteed 'false' value. *)
end
