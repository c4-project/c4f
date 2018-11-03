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

(** [Machine] contains the high-level interface for specifying and
    interacting with machines. *)

open Core
open Utils

(** [Reference] is the signature of references to machines. *)
module type Reference = sig
  (** [t] is the type of machine references. *)
  type t [@@deriving sexp]

  include Pretty_printer.S with type t := t

  (** [default] is the default machine reference, used whenever a
      reference is omitted in a compiler specification. *)
  val default : t

  (** [is_remote m] returns a guess as to whether machine reference
      [m] is a reference to a remote machine. *)
  val is_remote : t -> bool
end

(** [Ssh] is a module defining SSH configuration. *)
module Ssh : sig
  type t [@@deriving sexp]

  include Pretty_printer.S with type t := t

  (** [create ~host ?user ~copy_dir] builds an [Ssh.t] from the given
     parameters. *)
  val create : host:string -> ?user:string -> copy_dir:string -> t

  (** [host] gets the hostname of the SSH remote. *)
  val host : t -> string
  (** [user] gets the optional username of the SSH remote. *)
  val user : t -> string option
  (** [copy_dir] gets the remote directory to which we'll be
      copying work. *)
  val copy_dir : t -> string

  (** [To_config] lifts a [t] to an [Ssh.S]. *)
  module To_config : functor (C : sig val ssh : t end) -> Ssh.S
end

(** [Spec] is a module for machine specifications. *)
module Spec : sig
  (** [t] describes a machine. *)
  type t;;

  (** [via] describes the connection to the machine. *)
  type via =
    | Local
    | Ssh of Ssh.t
  ;;
  (** [via spec] gets the [via] stanza of a machine spec [spec]. *)
  val via : t -> via

  (** [to_runner spec] gets a [Run.runner] for the machine spec
     [spec]. *)
  val runner : t -> (module Run.Runner)

  (** Machine specifications are machine references... *)
  include Reference with type t := t
  (** ...and specifications. *)
  include Spec.S with type t := t
end
