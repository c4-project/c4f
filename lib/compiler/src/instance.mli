(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** High-level interface for specifying and invoking compilers. *)

open Base

module Fail (E : sig
  val error : Error.t
end) : Instance_types.S
(** A compiler that always passes its tests, but fails with [E.error] on
    runtime.

    Generally used when building a compiler chain fails, but the error is
    one that can be sidestepped over if the compiler never gets run. *)

module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S
(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)
