(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying various FIR primitives into an AST.

    For the big picture, see {!Reify}. *)

(** {1 Primitives} *)

val type_to_specs : Act_fir.Type.t -> [> Ast.Decl_spec.t] list
(** [type_to_specs ty] reifies the type [ty] to C declarator specs. *)

val decl : Act_fir.Initialiser.t Act_common.C_named.t -> Ast.Decl.t
(** [decl id d] reifies the mini-declaration [d] into the C AST. *)

val id_declarator : Act_fir.Type.t -> Act_common.C_id.t -> Ast.Declarator.t
(** [id_declarator ty id] constructs a declarator with a type [ty] and ID
    [id]. *)

val func_parameter : Act_fir.Type.t -> Act_common.C_id.t -> Ast.Param_decl.t
(** [func_parameter ty id] constructs a parameter declaration with a type
    [ty] and ID [id]. *)

(** {1 Expression sub-components} *)

val lvalue : Act_fir.Lvalue.t -> Ast.Expr.t
(** [lvalue l] reifies the lvalue [l] into the C AST. *)

val address : Act_fir.Address.t -> Ast.Expr.t
(** [address a] reifies the address [a] into the C AST. *)

val constant : Act_fir.Constant.t -> Ast.Expr.t
(** [constant k] reifies the constant [k] into the C AST. *)

(** {1 Pretty-printing} *)

val pp_constant : Act_fir.Constant.t Fmt.t
(** [pp_constant] pretty-prints constants via {!constant}. *)
