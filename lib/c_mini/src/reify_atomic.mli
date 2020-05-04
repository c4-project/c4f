(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a Mini-C atomic into an AST.

    These functions all take, recursively, an expression modeller. Usually,
    this should be {!Reify_expr.reify}. *)

val reify_expr :
     'e Atomic_expression.t
  -> expr:('e -> Act_c_lang.Ast.Expr.t)
  -> Act_c_lang.Ast.Expr.t
(** [reify_expr x ~expr] reifies an atomic expression. *)

val reify_stm :
     Atomic_statement.t
  -> expr:(Expression.t -> Act_c_lang.Ast.Expr.t)
  -> Act_c_lang.Ast.Stm.t
(** [reify_stm x ~expr] reifies an atomic statement. *)
