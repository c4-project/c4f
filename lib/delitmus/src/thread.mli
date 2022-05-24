(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmusifier: thread contexts. *)

open Base
open Import

(** Type of thread context. *)
type t = {tid: int; locals: Set.M(Common.C_id).t}

val when_local :
     t
  -> 'a
  -> over:(unit, Common.C_id.t, 'a, getter) Accessor.t
  -> f:('a -> 'a Or_error.t)
  -> 'a Or_error.t
(** [when_local t x ~over ~f] returns [f x] when [x.@(over)] is local in [t],
    and [x] otherwise. *)

val when_global :
     t
  -> 'a
  -> over:(unit, Common.C_id.t, 'a, getter) Accessor.t
  -> f:('a -> 'a Or_error.t)
  -> 'a Or_error.t
(** [when_local t x ~over ~f] returns [x] when [x.@(over)] is local in [t],
    and [f x] otherwise. *)
