(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [Ssh] contains types, modules, and functions for doing remote work over
    SSH. *)

open Base

(** {1 SSH configuration records} *)

type t [@@deriving sexp, equal]
(** [t] contains a SSH hostname and optional user. *)

(** {2 Constructors} *)

val make : ?user:string -> host:string -> unit -> t
(** [make ?user ~host ()] creates a [t] for connecting to [host], optionally
    as user [user]. *)

(** {2 Accessors} *)

val host : t -> string
(** [host ssh] gets the configured host for [ssh]. *)

val user : t -> string option
(** [user ssh] gets the configured user, if any, for [ssh]. *)

(** {1 Functors} *)

(** [Make] makes an [S] from a [t]. *)
module Make (Conf : sig
  val ssh : t
end) : Ssh_types.S

(** [Runner] provides a {!Runner_types.S} using the given SSH config. *)
module Runner (Conf : Ssh_types.Basic_runner) : Runner_types.S

(** [Scp] provides SCP file transfer operations, given an [S]. *)
module Scp (Conf : Ssh_types.S) : sig
  val send :
    recurse:bool -> local:Fpath.t -> remote:string -> unit Or_error.t
  (** [send ~recurse ~local ~remote] tries to copy the local path [local] to
      the remote host at path [remote] using scp. [recurse], if true, turns
      on the recursive copy flag. *)

  val receive :
    recurse:bool -> remote:string -> local:Fpath.t -> unit Or_error.t
  (** [receive ~recurse ~remote ~local] tries to copy the path [remote] on
      the remote host to the local path [local] using scp. [recurse], if
      true, turns on the recursive copy flag. *)
end
