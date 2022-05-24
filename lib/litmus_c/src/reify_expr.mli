(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying a FIR expression into an AST.

    For the big picture, see {!Reify}. *)

val reify : C4f_fir.Expression.t -> Ast.Expr.t
(** [reify e] reifies the mini-expression [e] into the C AST. *)

val pp : C4f_fir.Expression.t Fmt.t
(** [pp] is a pretty-printer via {!reify}. *)

(** {1 Helpers for reifying expressions}

    We expose these because they are useful in for-loop reification. *)
val postfix : Operators.Post.t -> Ast.Expr.t -> Ast.Expr.t
(** [postfix op x] builds a postfix expression with operator [op] and
    appropriately bracketed expression [x]. *)
