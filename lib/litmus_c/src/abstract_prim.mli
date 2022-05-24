(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Conversion of primitive expressions and statements from AST to FIR.

    Generally, something appears here if it's a primitive component that
    doesn't easily fit into an expression or statement box. *)

open Base

(** {1 Constants} *)

val constant : Ast_basic.Constant.t -> C4f_fir.Constant.t Or_error.t
(** [constant ast] tries to interpret a C constant AST as a FIR constant. *)

val identifier_to_constant : C4f_common.C_id.t -> C4f_fir.Constant.t option
(** [identifier_to_constant id] tries to interpret [id] as a (Boolean)
    constant. *)

(** {1 Declarators and declarations} *)

val sift_decls :
  ([> `Decl of 'd] as 'a) list -> ('d list * 'a list) Or_error.t
(** [sift_decls maybe_decl_list] tries to separate [maybe_decl_list] into a
    list of declarations followed immediately by a list of code, C89-style. *)

val decl :
  Ast.Decl.t -> C4f_fir.Initialiser.t C4f_common.C_named.t Or_error.t
(** [decl d] translates a declaration into an identifier-initialiser pair. *)

val param_decl :
  Ast.Param_decl.t -> C4f_fir.Type.t C4f_common.C_named.t Or_error.t
(** [param_decl d] translates a parameter declarator into an identifier-type
    pair. *)

(** {1 Converting expressions to lvalues, addresses, and so on} *)

val expr_to_identifier : Ast.Expr.t -> C4f_common.C_id.t Or_error.t
(** [expr_to_identifier e] tries to interpret [e] as an identifier. *)

val expr_to_lvalue : Ast.Expr.t -> C4f_fir.Lvalue.t Or_error.t
(** [expr_to_lvalue e] tries to interpret [e] as an lvalue. *)

val expr_to_address : Ast.Expr.t -> C4f_fir.Address.t Or_error.t
(** [expr_to_address e] tries to interpret [e] as an address. *)

val expr_to_memory_order : Ast.Expr.t -> C4f_fir.Mem_order.t Or_error.t
(** [expr_to_memory_order e] tries to interpret [e] as a memory order. *)
