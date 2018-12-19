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

  (** [id m] gets the machine ID of this reference. *)
  val id : t -> Id.t

  (** [remoteness m] returns a guess as to whether machine reference
      [m] is a reference to a remote machine. *)
  val remoteness : t -> [`Remote | `Local | `Unknown]
end

(** [Property] contains a mini-language for querying machine
   references, suitable for use in [Blang]. *)
module Property : sig
  (** [t] is the opaque type of property queries. *)
  type t [@@deriving sexp]

  (** [id] constructs a query over a machine's ID. *)
  val id : Id.Property.t -> t

  (** [is_remote] constructs a query that asks if a machine is
     known to be remote. *)
  val is_remote : t

  (** [is_local] constructs a query that asks if a machine is
      known to be local. *)
  val is_local : t

  (** [eval R reference property] evaluates [property] over
      [reference], with respect to module [R]. *)
  val eval
    :  (module Reference with type t = 'r)
    -> 'r
    -> t
    -> bool
  ;;

  (** [eval_b R reference expr] evaluates a [Blang] expression [expr]
     over [reference], with respect to module [R]. *)
  val eval_b
    :  (module Reference with type t = 'r)
    -> 'r
    -> t Blang.t
    -> bool
  ;;
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

(** [Id] is an extension onto base [Id] that
    lets such items be machine references. *)
module Id : sig
  include (module type of Id)
  include Reference with type t := t
end


(** [Via] enumerates the various methods of reaching a machine. *)
module Via : sig
  (** [t] is the type of a machine-reaching method. *)
  type t =
    | Local
    | Ssh of Ssh.t
  [@@deriving sexp]
  ;;

  (** [local] is a [Via] for a local machine. *)
  val local : t

  (** [ssh ssh_config] is a [Via] for a SSH connection. *)
  val ssh : Ssh.t -> t

  (** [t] can be pretty-printed. *)
  include Pretty_printer.S with type t := t

  (** [to_runner via] builds a [Run.Runner] module for a [via]. *)
  val to_runner : t -> (module Run.Runner)

  (** [remoteness via] gets an estimate of whether [via] is remote. *)
  val remoteness : t -> [> `Local | `Remote | `Unknown]
end

(** [Basic_spec] is the signature common to any sort of machine
    specification, including [With_id] pairs.

    In practice, modules implementing this will either be [Spec]
    or [Spec.With_id]. *)
module type Basic_spec = sig
  (** [t] describes a machine. *)
  type t

  (** [via spec] gets the [via] stanza of a machine spec [spec]. *)
  val via : t -> Via.t

  (** [to_runner spec] gets a [Run.runner] for the machine spec
     [spec]. *)
  val runner : t -> (module Run.Runner)
end

(** [Spec] is a module for machine specifications. *)
module Spec : sig
  include Basic_spec

 (** [create ~enabled ~via]
      creates a machine spec with the given fields.

      These fields are subject to change, and as such [create] is an
      unstable API. *)
  val create
    :  enabled : bool
    -> via     : Via.t
    -> t
  ;;

  (** [With_id] is an extension onto [Spec.With_id] that
      lets such items be machine references, and adds all of the
      [Spec] accessors. *)
  module With_id : sig
    include Spec.S_with_id with type elt := t
    include Basic_spec with type t := t
    include Reference with type t := t
  end

  (** Machine specifications are specifications. *)
  include Spec.S with type t := t and module With_id := With_id
end
