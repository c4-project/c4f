(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Ssh] contains types, modules, and functions for doing remote work
    over SSH. *)

open Core

(** [t] contains a SSH hostname and optional user. *)
type t

(** [host ssh] gets the configured host for [ssh]. *)
val host : t -> string

(** [user ssh] gets the configured user, if any, for [ssh]. *)
val user : t -> string option

(** [create ?user ~host] creates a [t] for connecting to [host],
    optionally as user [user]. *)
val create : ?user:string -> host:string -> t

(** [S] is the signature of modules that embed an SSH configuration. *)
module type S = sig
  (** [host] is the configured SSH host. *)
  val host : string

  (** [user] is the configured SSH user, if any. *)
  val user : string option
end

(** [Make] makes an [S] from a [t]. *)
module Make (Conf : sig val ssh : t end) : S

(** [Runner] provides a [Run.Runner] using the given SSH config. *)
module Runner (Conf : S) : Runner.S

(** [Scp] provides SCP file transfer operations, given an [S]. *)
module Scp (Conf : S) : sig
  (** [scp_send ~local ~remote] tries to copy the local path [local]
      to the remote host at path [remote] using
      scp. *)
  val send
    :  local  : string
    -> remote : string
    -> unit Or_error.t
  ;;

(** [receive ~remote ~local] tries to copy the path [remote] on
    the remote host to the local path [local] using
    scp. *)
  val receive
    :  remote : string
    -> local  : string
    -> unit Or_error.t
  ;;
end
