(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a Mini-C statement into an AST.

    For the big picture, see {!Reify}. *)

val reify : Expression.t -> Act_c_lang.Ast.Expr.t
(** [reify e] reifies the mini-expression [e] into the C AST. *)

(** {1 Expression sub-components} *)

val lvalue : Lvalue.t -> Act_c_lang.Ast.Expr.t
(** [lvalue l] reifies the lvalue [l] into the C AST. *)

val address : Address.t -> Act_c_lang.Ast.Expr.t
(** [address a] reifies the address [a] into the C AST. *)

val mem_order : Mem_order.t -> Act_c_lang.Ast.Expr.t
(** [mem_order m] reifies the memory order [m] into the C AST. *)

val known_call :
  string -> Act_c_lang.Ast.Expr.t list -> Act_c_lang.Ast.Expr.t
(** [known_call name args] reifies a function call to the function named
    [name] and with arguments [args] into the C AST. *)

val constant : Constant.t -> Act_c_lang.Ast.Expr.t
(** [constant k] reifies the constant [k] into the C AST. *)

(** {1 Atomic expressions} *)

module Atomic : sig
  val cmpxchg : Atomic_cmpxchg.t -> Act_c_lang.Ast.Expr.t
  (** [cmpxchg x] reifies a compare-exchange in expression position. *)
end
