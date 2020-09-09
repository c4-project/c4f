(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Known-output operator expression generation

    This module is a layer on top of the algebraic rules information about
    operators (see {!Op}, {!Op_rules}, and {!Op_types}), and provides generic
    functions for generating operations on expressions with specific output
    properties. *)

open Import

(** {1 Operand sets}

    We can generate a known-output operand expression with various types of
    input operands:

    - one expression (reflexive operations get that expression on both
      sides);
    - two equivalent expressions (reflexive operations get each expression
      but in a random order; and use-with-constant expressions get one of the
      two expressions at random).

    The {!Operand_set} module holds the type used to feed these operands into
    the generator. We assume a higher-level generator has already generated
    the expressions. *)

module Operand_set : sig
  type t =
    | One of Fir.Expression.t
    | Two of Fir.Expression.t * Fir.Expression.t
        (** Type of operand sets. *)
end

val bop :
     (module Fir.Op_types.S_binary with type t = 't)
  -> Operand_set.t
  -> promote:('t -> Fir.Op.Binary.t)
  -> out:Fir.Op_rule.Out.t
  -> Fir.Expression.t Base_quickcheck.Generator.t
(** [bop m operands ~promote ~out] generates a binary operation with operands
    drawn from [operands] and operators drawn from [m] via [promote], such
    that the output is guaranteed to match output rule [out]. *)
