(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Common functionality for regress tests *)

open Core_kernel

val regress_on_files :
     string
  -> dir:Fpath.t
  -> ext:string
  -> f:(file:Fpath.t -> path:Fpath.t -> unit Or_error.t)
  -> unit Or_error.t

val make_regress_command :
  (Fpath.t -> unit Or_error.t) -> summary:string -> Command.t
