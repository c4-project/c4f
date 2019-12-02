(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quick and easy process running *)

open Base

val argv_one_file :
     (string, 'a) Runner_types.argv_fun
  -> (string Copy_spec.t, 'a) Runner_types.argv_fun
(** [argv_one_file f] adapts a function that builds an argument list from one
    input file and one output file to one that works on manifests. *)

(** Makes a {{!S} S} from a {{!Basic} Basic}. *)
module Make (B : Runner_types.Basic) : Runner_types.S

(** {1 Basic runners}

    See also {!Ssh_runner}. *)

module Local : Runner_types.S
(** [Local] just runs commands on the local machine. *)

module Dry_run : Runner_types.S
(** [Dry_run] outputs each requested command to the supplied output channel
    rather than running it. *)
