(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a FIR atomic into an AST.

    These functions all take, recursively, an expression modeller. Usually,
    this should be {!Reify_expr.reify}. *)

val reify_expr :
  'e Act_fir.Atomic_expression.t -> expr:('e -> Ast.Expr.t) -> Ast.Expr.t
(** [reify_expr x ~expr] reifies an atomic expression. *)

val reify_stm :
     Act_fir.Atomic_statement.t
  -> expr:(Act_fir.Expression.t -> Ast.Expr.t)
  -> Ast.Stm.t
(** [reify_stm x ~expr] reifies an atomic statement. *)
