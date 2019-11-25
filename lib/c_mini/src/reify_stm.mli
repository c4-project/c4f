(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a Mini-C statement into an AST.

    For the big picture, see {!Reify}. *)

val reify : _ Statement.t -> Act_c_lang.Ast.Stm.t
(** [reify s] reifies the mini-statement [s] into the C AST. *)

val reify_compound : _ Statement.t list -> Act_c_lang.Ast.Compound_stm.t
(** [reify_compound xs] reifies a list [xs] of statements into a C AST
    compound statement. *)
