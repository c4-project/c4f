(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

val type_to_specs : Type.t -> [> Act_c_lang.Ast.Decl_spec.t] list
(** [type_to_specs ty] reifies the type [ty] to C declarator specs. *)

val decl : Initialiser.t Act_common.C_named.t -> Act_c_lang.Ast.Decl.t
(** [decl id d] reifies the mini-declaration [d] into the C AST. *)

val id_declarator :
  Type.t -> Act_common.C_id.t -> Act_c_lang.Ast.Declarator.t
(** [id_declarator ty id] constructs a declarator with a type [ty] and ID
    [id]. *)
