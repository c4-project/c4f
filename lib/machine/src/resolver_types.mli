(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for resolvers. *)

open Core_kernel

module type S_compiler = sig
  val resolve :
       Qualified.Compiler.t
    -> (module Act_compiler.Instance_types.S) Or_error.t

  val filtered_list :
       ?machine_predicate:Property.t Blang.t
    -> ?compiler_predicate:Act_compiler.Property.t Blang.t
    -> ?test_compilers:bool
    -> Spec.Set.t
    -> Qualified.Compiler.t Resolver_listing.t
end
