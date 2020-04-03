(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Modules that run programs in various ways.

    This module, and its siblings {!Runner_types} and {!Ssh_runner}, describe
    a module type of 'runners', which expose functions for running programs
    on local or remote machines.

    To support remote running, runners have an optional concept of {{!copy
    specs} Copy_spec}, which describe a working set (a directory, a set of
    files, etc.) that must be copied from the local machine to a remote
    machine. When running a command with a copy spec, the runner parametrises
    the construction of an argument vector (and, optionally, the program
    name) over the spec. *)

(** {1 Functors} *)

(** Makes a {!S} from a {!Basic}. *)
module Make (B : Runner_types.Basic) : Runner_types.S

(** {1 Basic runners}

    See also {!Ssh_runner}. *)

(** [Local] just runs commands on the local machine. *)
module Local : Runner_types.S

(** [Dry_run] outputs each requested command to the supplied output channel
    rather than running it. *)
module Dry_run : Runner_types.S

(** {1 Helpers for using runners} *)

(** {2 Program functions}

    These slot into the [prog_f] optional parameter of the [run_with_copy]
    functions. *)

val id_prog : Runner_types.prog_fun
(** [id_prog] is a [prog_f] that doesn't alter the program at all. *)

val copy_prog : Runner_types.prog_fun
(** [copy_prog] is a [prog_f] that checks whether the current program is in
    the input copy spec, and, if it is, replaces it with its projection into
    the remote namespace. This is useful for doing remote program execution. *)
