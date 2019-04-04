(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Ssh] contains types, modules, and functions for doing remote work over
    SSH. *)

open Core

include module type of Ssh_intf

(** {2 SSH configuration records} *)

(** [t] contains a SSH hostname and optional user. *)
type t

val host : t -> string
(** [host ssh] gets the configured host for [ssh]. *)

val user : t -> string option
(** [user ssh] gets the configured user, if any, for [ssh]. *)

val make : ?user:string -> host:string -> unit -> t
(** [make ?user ~host ()] creates a [t] for connecting to [host], optionally
    as user [user]. *)

(** {2 Functors} *)

(** [Make] makes an [S] from a [t]. *)
module Make (Conf : sig
  val ssh : t
end) : S

(** [Runner] provides a [Run.Runner] using the given SSH config. *)
module Runner (Conf : Basic_runner) : Runner.S

(** [Scp] provides SCP file transfer operations, given an [S]. *)
module Scp (Conf : S) : sig
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
