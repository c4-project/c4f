(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Machine SSH configuration. *)

open Base

type t [@@deriving sexp, equal]

include Pretty_printer.S with type t := t

val make : ?user:string -> host:string -> copy_dir:string -> unit -> t
(** [make ?user ~host ~copy_dir ()] builds an [Ssh.t] from the given
    parameters. *)

val host : t -> string
(** [host] gets the hostname of the SSH remote. *)

val user : t -> string option
(** [user] gets the optional username of the SSH remote. *)

val copy_dir : t -> string
(** [copy_dir] gets the remote directory to which we'll be copying work. *)

(** [To_config] lifts a [t] to an [Ssh.S]. *)
module To_config (C : sig
  val ssh : t
end) : Act_utils.Ssh.S
