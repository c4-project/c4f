(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a FIR atomic into an AST.

    These functions all take, recursively, an expression modeller. Usually,
    this should be {!Reify_expr.reify}. *)

val reify_expr :
  'e C4f_fir.Atomic_expression.t -> expr:('e -> Ast.Expr.t) -> Ast.Expr.t
(** [reify_expr x ~expr] reifies an atomic expression. *)

val reify_stm :
     C4f_fir.Atomic_statement.t
  -> expr:(C4f_fir.Expression.t -> Ast.Expr.t)
  -> Ast.Stm.t
(** [reify_stm x ~expr] reifies an atomic statement. *)
