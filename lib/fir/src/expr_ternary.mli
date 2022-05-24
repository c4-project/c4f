(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Ternary expressions. *)

open Import

(** Type of ternary expressions, parametric over inner expressions. *)
type 'e t = {if_: 'e; then_: 'e; else_: 'e}
[@@deriving sexp, accessors, compare, equal, quickcheck]

val exprs :
  ('i -> 'e1 -> 'e2, 'i -> 'e1 t -> 'e2 t, [< many]) Accessor.General.t
(** [exprs] focuses on the expressions inside a ternary expression. *)

val quickcheck_generator_ite :
     gen_if:'e Q.Generator.t
  -> gen_then:'e Q.Generator.t
  -> gen_else:'e Q.Generator.t
  -> 'e t Q.Generator.t
(** [quickcheck_generator_ite ~gen_if ~gen_then ~gen_else] is a more
    fine-grained quickcheck generator that allows customising of each of the
    expression generators. *)

(** Type checking for ternary expressions. *)
module Type_check (E : Env_types.S) :
  Types.S_type_checker with type t := Type.t t
