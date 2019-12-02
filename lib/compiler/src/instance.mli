(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** High-level interface for specifying and invoking compilers. *)

(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)
module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S
