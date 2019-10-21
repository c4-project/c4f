(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a mini-model into an AST. *)

val func :
  Act_common.C_id.t -> _ Function.t -> Act_c_lang.Ast.External_decl.t
(** [func id f] reifies the mini-function [f], with name [id], into the C
    AST. *)

val program : _ Program.t -> Act_c_lang.Ast.Translation_unit.t
(** [program p] reifies the mini-program [p] into the C AST. *)

val decl : Act_common.C_id.t -> Initialiser.t -> Act_c_lang.Ast.Decl.t
(** [decl id d] reifies the mini-declaration [d], with name [id], into the C
    AST. *)

val expr : Expression.t -> Act_c_lang.Ast.Expr.t
(** [expr e] reifies the mini-expression [e] into the C AST. *)

val stm : _ Statement.t -> Act_c_lang.Ast.Stm.t
(** [stm s] reifies the mini-statement [s] into the C AST. *)
