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

val to_string : t -> string
(** [to_string ssh] converts [ssh] to a string. This string is intended to be
    suitable for prefixing a SCP remote. *)

include Pretty_printer.S with type t := t
(** SSH configuration records can be pretty-printed. *)

(** {2 Constructors} *)

val make : ?user:string -> host:string -> unit -> t
(** [make ?user ~host ()] creates a [t] for connecting to [host], optionally
    as user [user]. *)

(** {2 Accessors} *)

val host : t -> string
(** [host ssh] gets the configured host for [ssh]. *)

val user : t -> string option
(** [user ssh] gets the configured user, if any, for [ssh]. *)

(** {2 Running commands} *)

val run :
     ?out:Runner_output.t
  -> t
  -> prog:string
  -> argv:string list
  -> unit Or_error.t
(** [run ?out ssh ~prog ~argv] is a wrapper around Jane Street's [Shell]
    library's SSH support. It invokes [prog] with arguments [argv] on the
    remote host and user given by [ssh]. If [out] is supplied, the final
    output from the command gets printed to it. *)
