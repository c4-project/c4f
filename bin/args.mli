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

(** Argument specifications common to all act sub-commands. *)

open Core
open Lib
open Utils

include module type of Args_intf

(** {2 The standard arguments} *)

module Standard : S_standard
(** Module implementing a common way to retrieve and work with the
   act standard argument set. *)

(** Variant of {{!Standard}Standard} including arguments for
    (optional) input and output files.  This is useful for exposing
    a filter as an act command. *)
module Standard_with_files : sig
  type t
  (** Opaque type of processed argument records. *)

  include S_standard with type t := t

  val as_standard_args : t -> Standard.t
  (** [as_standard_args args] extracts a {{!Standard.t}Standard.t}
     from [args]. *)

  (** {3 Retrieving input and output files } *)

  val infile_raw : t -> string option
  (** [infile_raw args] gets the input file as provided as an
     argument, if one indeed was. *)

  val infile_fpath : t -> Fpath.t option Or_error.t
  (** [infile_fpath args] behaves as {{!infile_raw}infile_raw}, but
     tries to parse any given input file as an Fpath.  This may fail
     if the path is ill-formed. *)

  val infile_source : t -> Io.In_source.t Or_error.t
  (** [infile_source args] behaves as {{!infile_raw}infile_raw}, but
     tries to convert the result to an {{!Io.In_source.t}In_source.t}.
     This may fail if the path is ill-formed. *)

  val outfile_raw : t -> string option
  (** [outfile_raw args] gets the output file as provided as an
     argument, if one indeed was. *)

  val outfile_fpath : t -> Fpath.t option Or_error.t
  (** [outfile_fpath args] behaves as {{!outfile_raw}outfile_raw}, but
     tries to parse any given output file as an Fpath.  This may fail
     if the path is ill-formed. *)

  val outfile_sink : t -> Io.Out_sink.t Or_error.t
  (** [outfile_sink args] behaves as {{!outfile_raw}outfile_raw}, but
     tries to convert the result to an {{!Io.Out_sink.t}Out_sink.t}.
     This may fail if the path is ill-formed. *)
end

(** {2 Miscellaneous argument helpers} *)

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
