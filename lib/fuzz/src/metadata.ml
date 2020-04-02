(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {source: [`Existing | `Generated]; liveness: [`Normal | `Dead]}
[@@deriving sexp, compare, equal]

let existing : t = {source= `Existing; liveness= `Normal}

let generated : t = {source= `Generated; liveness= `Normal}

let dead_code : t = {source= `Generated; liveness= `Dead}

let is_dead_code : t -> bool = function
  | {liveness= `Dead; _} ->
      true
  | _ ->
      false
