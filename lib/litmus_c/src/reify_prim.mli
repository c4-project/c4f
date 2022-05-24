(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying various FIR primitives into an AST.

    For the big picture, see {!Reify}. *)

(** {1 Primitives} *)

val type_to_specs : C4f_fir.Type.t -> [> Ast.Decl_spec.t] list
(** [type_to_specs ty] reifies the type [ty] to C declarator specs. *)

val decl : C4f_fir.Initialiser.t C4f_common.C_named.t -> Ast.Decl.t
(** [decl id d] reifies the mini-declaration [d] into the C AST. *)

val id_declarator : C4f_fir.Type.t -> C4f_common.C_id.t -> Ast.Declarator.t
(** [id_declarator ty id] constructs a declarator with a type [ty] and ID
    [id]. *)

val func_parameter : C4f_fir.Type.t -> C4f_common.C_id.t -> Ast.Param_decl.t
(** [func_parameter ty id] constructs a parameter declaration with a type
    [ty] and ID [id]. *)

(** {1 Expression sub-components} *)

val lvalue : C4f_fir.Lvalue.t -> Ast.Expr.t
(** [lvalue l] reifies the lvalue [l] into the C AST. *)

val address : C4f_fir.Address.t -> Ast.Expr.t
(** [address a] reifies the address [a] into the C AST. *)

val constant : C4f_fir.Constant.t -> Ast.Expr.t
(** [constant k] reifies the constant [k] into the C AST. *)

(** {1 Pretty-printing} *)

val pp_constant : C4f_fir.Constant.t Fmt.t
(** [pp_constant] pretty-prints constants via {!constant}. *)
