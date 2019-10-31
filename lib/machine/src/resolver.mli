(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

module Compiler (Resolver : sig
  val f :
       Act_compiler.Spec.With_id.t
    -> (module Act_compiler.Instance_types.Basic) Or_error.t
end) : Resolver_types.S_compiler
(** Constructs a compiler resolver, given a basic compiler resolution
    function. *)
