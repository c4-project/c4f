(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Support for probing a machine for its spec, given information on how to
    connect to it. *)

open Base

val probe :
     ?c_model:string
  -> Via.t
  -> compiler_styles:
       ( Act_common.Id.t
       , (module Act_compiler.Instance_types.Basic) )
       List.Assoc.t
  -> backend_styles:
       ( Act_common.Id.t
       , (module Act_backend.Instance_types.Basic) )
       List.Assoc.t
  -> Spec.t Or_error.t
(** [probe ?c_model via ~compiler_styles ~backend_styles] tries to create a
    spec for the machine reachable by [via] through scanning for the
    compilers in [compiler_styles] and backends in [backend_styles].

    Where appropriate and provided, the C model override [c_model] will
    filter down to backend specifications. *)
