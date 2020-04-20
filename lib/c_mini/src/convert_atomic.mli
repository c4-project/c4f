(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Conversion of atomics from AST to C-mini.

    In the AST, these atomics appear as function calls in both expression and
    statement position; in C-mini, they have distinct representations. *)

open Base

(** {1 Names of functions} *)

val cmpxchg_name : string
(** [cmpxchg_name] is the name of the atomic compare-exchange function. *)

val fence_signal_name : string
(** [fence_signal_name] is the name of the atomic signal fence function. *)

val fence_thread_name : string
(** [fence_thread_name] is the name of the atomic thread fence function. *)

val fetch_add_name : string
(** [fetch_add_name] is the name of the atomic fetch-add function. *)

val fetch_sub_name : string
(** [fetch_sub_name] is the name of the atomic fetch-sub function. *)

val load_name : string
(** [load_name] is the name of the atomic load function. *)

val store_name : string
(** [store_name] is the name of the atomic load function. *)

(** {1 Modellers} *)

val model_fence : Act_c_lang.Ast.Expr.t list -> mode:Atomic_fence.Mode.t ->
    Atomic_fence.t Or_error.t
(** [model_fence args ~mode] tries to convert an atomic-fence function call with
    arguments [args] and mode [mode] into an atomic fence. *)

val model_load : Act_c_lang.Ast.Expr.t list -> Atomic_load.t Or_error.t
(** [model_load args] tries to convert an atomic-load function call with
    arguments [args] into an atomic load. *)

(** {2 Modellers with recursive expressions} *)

val model_cmpxchg : Act_c_lang.Ast.Expr.t list -> expr:(Act_c_lang.Ast.Expr.t -> Expression.t Or_error.t) -> Expression.t Atomic_cmpxchg.t Or_error.t
(** [model_cmpxchg args ~expr] tries to convert an atomic compare-exchange
    function call with arguments [args] into an atomic compare-exchange, using
    [expr] to model inner expressions. *)

val model_fetch : Act_c_lang.Ast.Expr.t list -> op:Op.Fetch.t -> expr:(Act_c_lang.Ast.Expr.t -> Expression.t Or_error.t) -> Expression.t Atomic_fetch.t Or_error.t
(** [model_fetch args ~op ~expr] tries to convert an atomic fetch
    function call with arguments [args] and operation [op] into an atomic compare-exchange, using
    [expr] to model inner expressions. *)

val model_store : Act_c_lang.Ast.Expr.t list -> expr:(Act_c_lang.Ast.Expr.t -> Expression.t Or_error.t) -> Atomic_store.t Or_error.t
(** [model_store args ~expr] tries to convert an atomic store
    function call with arguments [args] into an atomic store, using
    [expr] to model inner expressions. *)
