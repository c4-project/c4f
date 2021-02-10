(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Conversion of atomics from AST to FIR.

    In the AST, these atomics appear as function calls in both expression and
    statement position; in FIR, they have distinct representations. *)

open Base
open Import

(** {1 Names of functions} *)

val cmpxchg_name : Fir.Atomic_cmpxchg.Strength.t -> string
(** [cmpxchg_name] is the name of the atomic compare-exchange function. *)

val fence_name : Fir.Atomic_fence.Mode.t -> string
(** [fence_name mode] is the name of the atomic fence function for mode
    [mode]. *)

val fetch_name : Fir.Op.Fetch.t -> string
(** [fetch_name f] is the name of the atomic fetch-[f] function (or
    atomic-exchange, if [f] is [`Xchg]). *)

val load_name : string
(** [load_name] is the name of the atomic load function. *)

val store_name : string
(** [store_name] is the name of the atomic store function. *)

(** {1 Modeller helpers} *)

val cmpxchg_call_alist :
     (Ast.Expr.t list -> strength:Fir.Atomic_cmpxchg.Strength.t -> 'a)
  -> (C4f_common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t
(** [cmpxchg_call_alist f] builds an associative list mapping cmpxchg
    known-calls to modellers over their argument lists, using [f] to
    construct those modellers. *)

val fence_call_alist :
     (Ast.Expr.t list -> mode:Fir.Atomic_fence.Mode.t -> 'a)
  -> (C4f_common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t
(** [fence_call_alist f] builds an associative list mapping fence known-calls
    to modellers over their argument lists, using [f] to construct those
    modellers. *)

val fetch_call_alist :
     (Ast.Expr.t list -> op:Fir.Op.Fetch.t -> 'a)
  -> (C4f_common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t
(** [fetch_call_alist f] builds an associative list mapping fetch known-calls
    to modellers over their argument lists, using [f] to construct those
    modellers. *)

(** {1 Modellers} *)

val model_fence :
     Ast.Expr.t list
  -> mode:Fir.Atomic_fence.Mode.t
  -> Fir.Atomic_fence.t Or_error.t
(** [model_fence args ~mode] tries to convert an atomic-fence function call
    with arguments [args] and mode [mode] into an atomic fence. *)

val model_load : Ast.Expr.t list -> Fir.Atomic_load.t Or_error.t
(** [model_load args] tries to convert an atomic-load function call with
    arguments [args] into an atomic load. *)

(** {2 Modellers with recursive expressions} *)

val model_cmpxchg :
     Ast.Expr.t list
  -> strength:Fir.Atomic_cmpxchg.Strength.t
  -> expr:(Ast.Expr.t -> Fir.Expression.t Or_error.t)
  -> Fir.Expression.t Fir.Atomic_cmpxchg.t Or_error.t
(** [model_cmpxchg args ~strength ~expr] tries to convert an atomic
    compare-exchange function call with arguments [args] into an atomic
    compare-exchange of strength [strength], using [expr] to model inner
    expressions. *)

val model_fetch :
     Ast.Expr.t list
  -> op:Fir.Op.Fetch.t
  -> expr:(Ast.Expr.t -> Fir.Expression.t Or_error.t)
  -> Fir.Expression.t Fir.Atomic_fetch.t Or_error.t
(** [model_fetch args ~op ~expr] tries to convert an atomic fetch (or
    exchange) function call with arguments [args] and operation [op] into an
    atomic compare-exchange, using [expr] to model inner expressions. *)

val model_store :
     Ast.Expr.t list
  -> expr:(Ast.Expr.t -> Fir.Expression.t Or_error.t)
  -> Fir.Atomic_store.t Or_error.t
(** [model_store args ~expr] tries to convert an atomic store function call
    with arguments [args] into an atomic store, using [expr] to model inner
    expressions. *)
