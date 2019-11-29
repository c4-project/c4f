(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Machine SSH configuration. *)

open Base

(** {1 Configuration records}

    These combine a {!Plumbing.Ssh.t} with an extra parameter giving the name
    of a directory on the remote to which temporary files may be copied. *)

type t [@@deriving sexp, equal]

include Pretty_printer.S with type t := t

(** {2 Constructors} *)

val make : remote:Plumbing.Ssh.t -> copy_dir:string -> t
(** [make ~remote ~copy_dir] builds a {!t} from the given parameters. *)

(** {2 Accessors} *)

val remote : t -> Plumbing.Ssh.t
(** [remote ssh] gets [ssh]'s remote configuration. *)

val copy_dir : t -> string
(** [copy_dir ssh] gets, from [ssh], the remote directory to which we'll be
    copying work. *)

(** {3 Shortcuts for accessing SSH remote information} *)

val host : t -> string
(** [host ssh] gets the hostname of the SSH remote of [ssh]. *)

val user : t -> string option
(** [user ssh] gets the optional username of the SSH remote of [ssh]. *)

(** [To_config] lifts a [t] to a {!Plumbing.Ssh_types.S}. *)
module To_config (C : sig
  val ssh : t
end) : Plumbing.Ssh_types.S
