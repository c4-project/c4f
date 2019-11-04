(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Lookup services for machine components *)

open Base

module Compiler (Basic : sig
  val test : Qualified.Compiler.t -> unit Or_error.t
end) : Lookup_types.S_compiler
(** Constructs a compiler lookup given a compiler presence testing function. *)

module Backend (Resolver : sig
  val test : Qualified.Sim.t -> unit Or_error.t
end) : Lookup_types.S_backend
(** Constructs a backend lookup given a backend presence testing function. *)
