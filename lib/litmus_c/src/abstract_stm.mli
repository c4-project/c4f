(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstracting statement ASTs into FIR.

    See {!Abstract} for the big picture. *)

open Base

val ensure_statements :
  Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t
(** [ensure_statements xs] makes sure that each member of [xs] is a
    statement. *)

val model : Ast.Stm.t -> unit Act_fir.Statement.t Or_error.t
(** [model ast] tries to model a C statement AST as a FIR statement. *)
