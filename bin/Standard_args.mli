(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** [Standard_args] contains argument specifications common to all act
   sub-commands. *)

open Core
open Lib

(** [t] collects all of the standard arguments in one record. *)
type t

(** [is_verbose t] gets whether, according to [t], verbose mode is
   switched on. *)
val is_verbose : t -> bool

(** [are_warnings_enabled t] gets whether, according to [t], warnings are
   switched on. *)
val are_warnings_enabled : t -> bool

(** [config_file t] gets the configuration file according to [t]. *)
val config_file : t -> string

(** [get] is a [Command.Param.t] that describes how to get the standard
    arguments at the command line. *)
val get : t Command.Param.t

(** [Other] collects parameter specifications for arguments that aren't
    as common as the standard ones, but are still used in more than
    one sub-command. *)
module Other : sig
  val flag_to_enum_choice
    : 'a -> string -> doc:string -> 'a option Command.Param.t
  (** [flag_to_enum_choice enum str ~doc] is a helper for implementing
     choose-one choices between multiple flags where each flag [str]
      corresponds to an enum variant [enum]. *)

  val arch
    : ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
  (** [arch ?name ?doc ()] produces a parameter, normally named [-arch]
      but overridable by [name], that accepts an architecture ID. *)

  val compiler_id_or_arch
    : [> `Arch of Id.t | `Id of Id.t] Command.Param.t
  (** [compiler_id_or_arch] defines a choice between supplying a
      compiler ID, or a direct architecture. *)

  val file_type : [> `C_litmus | `Assembly | `C | `Infer] Command.Param.t
  (** [file_type] defines a parameter for specifying the file type of
      a single input file. *)

  val c_symbols : string list option Command.Param.t
  (** [c_symbols] defines a parameter for collecting a list of
      C symbols to track during sanitisation. *)

  val sanitiser_passes
    : Sanitiser_pass.Selector.t Blang.t option Command.Param.t
  (** [sanitiser_passes] defines a parameter for collecting a selector
     predicate for sanitiser passes. *)

  val compiler_predicate
    : Compiler.Property.t Blang.t option Command.Param.t
  (** [compiler_predicate] defines a parameter for collecting a
      filtering predicate for compilers. *)

  val machine_predicate
    : Machine.Property.t Blang.t option Command.Param.t
  (** [machine_predicate] defines a parameter for collecting a
      filtering predicate for machines. *)
end
