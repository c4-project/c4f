(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t = {tid: int; locals: Set.M(Common.C_id).t}

let is_local ({locals; _} : t) : Common.C_id.t -> bool = Set.mem locals

let when_local (t : t) (v : 'a)
    ~(over : (unit, Common.C_id.t, 'a, getter) Accessor.Simple.t)
    ~(f : 'a -> 'a Or_error.t) : 'a Or_error.t =
  if is_local t v.@(over) then f v else Ok v

let when_global (t : t) (v : 'a)
    ~(over : (unit, Common.C_id.t, 'a, getter) Accessor.Simple.t)
    ~(f : 'a -> 'a Or_error.t) : 'a Or_error.t =
  if is_local t v.@(over) then Ok v else f v
