(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Evaluating expressions.

    This module provides a rundimentary expression evaluator for FIR
    expressions. This evaluator assumes that we can model the environment as
    a {!Heap}, which is quite simplistic in the face of weak memory. *)

open Base

val as_constant : Expression.t -> env:Heap.t -> Constant.t Or_error.t
(** [as_constant expr ~env] evaluates expression [expr] in the context of the
    abstract environment (store/heap) model [env], returning a constant if
    the evaluation succeeded or an error otherwise. *)
