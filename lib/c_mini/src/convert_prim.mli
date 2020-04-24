(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Conversion of primitive expressions and statements from AST to C-mini.

    Generally, something appears here if it's a primitive component that
    doesn't easily fit into an expression or statement box. *)

open Base

val constant : Act_c_lang.Ast_basic.Constant.t -> Constant.t Or_error.t
(** [constant ast] tries to interpret a C constant AST as a mini-model
    constant. *)

(** {1 Declarators and declarations} *)

val decl :
  Act_c_lang.Ast.Decl.t -> Initialiser.t Act_common.C_named.t Or_error.t
(** [decl d] translates a declaration into an identifier-initialiser pair. *)

val param_decl :
  Act_c_lang.Ast.Param_decl.t -> Type.t Act_common.C_named.t Or_error.t
(** [param_decl d] translates a parameter declarator into an identifier-type
    pair. *)

(** {1 Converting expressions to lvalues, addresses, and so on} *)

val expr_to_identifier :
  Act_c_lang.Ast.Expr.t -> Act_common.C_id.t Or_error.t
(** [expr_to_identifier e] tries to interpret [e] as an identifier. *)

val expr_to_lvalue : Act_c_lang.Ast.Expr.t -> Lvalue.t Or_error.t
(** [expr_to_lvalue e] tries to interpret [e] as an lvalue. *)

val expr_to_address : Act_c_lang.Ast.Expr.t -> Address.t Or_error.t
(** [expr_to_address e] tries to interpret [e] as an address. *)

val expr_to_memory_order : Act_c_lang.Ast.Expr.t -> Mem_order.t Or_error.t
(** [expr_to_memory_order e] tries to interpret [e] as a memory order. *)
