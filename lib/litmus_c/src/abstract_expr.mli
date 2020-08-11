(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstracting expression ASTs into FIR.

    See {!Abstract} for the big picture. *)

open Base

val model : Ast.Expr.t -> Act_fir.Expression.t Or_error.t
(** [model ast] tries to model a C expression AST as a FIR statement. *)
