(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Enumerates the various methods of reaching a machine. *)

open Base

(** [t] is the type of a machine-reaching method. *)
type t = Local | Ssh of Ssh.t [@@deriving sexp, equal]

val local : t
(** [local] is a [Via] for a local machine. *)

val ssh : Ssh.t -> t
(** [ssh ssh_config] is a [Via] for a SSH connection. *)

(** [t] can be pretty-printed. *)
include Pretty_printer.S with type t := t

val to_runner : t -> (module Plumbing.Runner_types.S)
(** [to_runner via] builds a runner module for a [via]. *)

val remoteness : t -> [> `Local | `Remote | `Unknown]
(** [remoteness via] gets an estimate of whether [via] is remote. *)
