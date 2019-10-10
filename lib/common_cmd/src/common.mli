(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Glue code common to all top-level commands. *)

open Core_kernel
open Act_common

val lift_command :
     Args.Standard.t
  -> f:(Output.t -> Act_config.Global.t -> unit Or_error.t)
  -> unit
(** [lift_command standard_args ~f] lifts a command body [f], performing
    common book-keeping such as loading and testing the configuration,
    creating an [Output.t], and printing top-level errors. *)
