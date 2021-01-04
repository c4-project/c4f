(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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

val basic_lift_k : Fir.Constant.t -> Fir.Expression.t Q.Generator.t
(** [basic_lift_k k] lifts constants into singleton expression generators. *)

(** Type of partially applied bop generators. *)
type bop_gen =
     (Fir.Constant.t -> Fir.Expression.t Q.Generator.t)
  -> Operand_set.t
  -> Fir.Expression.t Q.Generator.t

val bop_with_output :
  ?ops:Fir.Op.Binary.t list -> Fir.Op_rule.Out.t -> bop_gen option
(** [bop_with_output ?ops ~out] tries to produce a generator generating a
    binary operation from [ops] (defaulting to all operators) such that the
    output is guaranteed to match output rule [out]; it returns [None] if
    there are no viable operators. The returned generator should provide an
    operand set and lifting generator for any constants involved in the rule. *)
