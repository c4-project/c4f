(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** Opaque type of delitmus context. *)
type t

(** {2 Constructors} *)

val make :
     aux:Aux.t
  -> local_inits:
       (int, (Common.C_id.t, Fir.Constant.t) List.Assoc.t) List.Assoc.t
  -> t
(** [make ~aux ~local_inits] makes a delitmus context. *)

(** {2 Components} *)

val aux : t -> Aux.t
(** [aux ctx] gets the auxiliary record in [ctx]. *)

val var_map : t -> Var_map.t
(** [var_map ctx] gets the variable map in [ctx]. *)

val lookup_initial_value :
  t -> id:Common.Litmus_id.t -> Fir.Constant.t option
(** [lookup_initial_value ctx ~id] gets the initial assignment for [id] in
    the litmus test, if one exists. *)
