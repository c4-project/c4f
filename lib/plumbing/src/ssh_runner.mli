(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functors and types for defining {{!Runner_types.S} runners} that operate
    by SCPing input files to a remote server, SSHing a command over to that
    server, and SCPing output files beck. *)

open Base

(** {1 Configuration records}

    These combine a {!Ssh.t} with an extra parameter giving the name of a
    directory on the remote to which temporary files may be copied. *)

module Config : sig
  type t [@@deriving sexp, equal]

  include Pretty_printer.S with type t := t

  (** {2 Constructors} *)

  val make : remote:Ssh.t -> copy_dir:string -> t
  (** [make ~remote ~copy_dir] builds a {!t} from the given parameters. *)

  val of_string : string -> t
  (** [of_string s] parses [s] as a {!t}. The string should be of the form
      user\@host:copy_dir, where user\@ is optional. *)

  (** {2 Accessors} *)

  val remote : t -> Ssh.t
  (** [remote ssh] gets [ssh]'s remote configuration. *)

  val copy_dir : t -> string
  (** [copy_dir ssh] gets, from [ssh], the remote directory to which we'll be
      copying work. *)

  (** {3 Shortcuts for accessing SSH remote information} *)

  val host : t -> string
  (** [host ssh] gets the hostname of the SSH remote of [ssh]. *)

  val user : t -> string option
  (** [user ssh] gets the optional username of the SSH remote of [ssh]. *)
end

(** {1 Constructing runners} *)

module Make (C : sig
  val config : Config.t
end) : Runner_types.S
