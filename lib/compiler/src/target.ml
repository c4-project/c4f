(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Q_spec = Machine_spec.Qualified_compiler

type t = [`Spec of Q_spec.t | `Arch of Ac.Id.t]

let arch : t -> Ac.Id.t = function
  | `Spec spec ->
      Spec.With_id.emits (Q_spec.c_spec spec)
  | `Arch arch ->
      arch

let ensure_spec : t -> Q_spec.t Or_error.t = function
  | `Spec spec ->
      Or_error.return spec
  | `Arch _ ->
      Or_error.error_string
        "Expected a compiler ID; got an architecture ID."
