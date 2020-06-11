(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying a FIR expression into an AST.

    For the big picture, see {!Reify}. *)

val reify : Expression.t -> Act_c_lang.Ast.Expr.t
(** [reify e] reifies the mini-expression [e] into the C AST. *)
