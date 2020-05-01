(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a Mini-C program into an AST.

    These functions reify various top-level elements of a Mini-C program.
    Other modules govern smaller components:

    - {!Reify_prim} for miscellaneous primitives;
    - {!Reify_expr} for expressions;
    - {!Reify_stm} for statements. *)

val func :
  Act_common.C_id.t -> _ Function.t -> Act_c_lang.Ast.External_decl.t
(** [func id f] reifies the mini-function [f], with name [id], into the C
    AST. *)

val program : _ Program.t -> Act_c_lang.Ast.Translation_unit.t
(** [program p] reifies the mini-program [p] into the C AST. *)
