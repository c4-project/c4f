(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Argument specifications common to all act sub-commands. *)

open Core_kernel
open Act_common

(** {2 The standard arguments} *)

(** Module implementing a common way to retrieve and work with the act
    standard argument set. *)
module Standard : sig
  type t
  (** Opaque type of processed argument records. *)

  val get : t Command.Param.t
  (** [get] is a [Command.Param.t] that describes how to get the standard
      arguments at the command line. *)

  val is_verbose : t -> bool
  (** [is_verbose t] gets whether, according to [t], verbose mode is
      switched on. *)

  val are_warnings_enabled : t -> bool
  (** [are_warnings_enabled t] gets whether, according to [t], warnings are
      switched on. *)

  val colour : t -> Fmt.style_renderer option
  (** [colour t] gets the TTY colouring mode, if any, according to [t]. *)

  val config_file : t -> string
  (** [config_file t] gets the configuration file according to [t]. *)
end

(** Wrapper of {{!Standard} Standard} (etc) including arguments for
    (optional) input and output files. This is useful for exposing a filter
    as an act command. *)
module With_files : sig
  type 'a t
  (** Opaque type of processed argument records. *)

  val rest : 'a t -> 'a
  (** [rest args] retrieves the argument record wrapped by [args]. *)

  val get : 'a Command.Param.t -> 'a t Command.Param.t
  (** [get] lifts a [Command.Param.t] over arguments to one that also
      retrieves the file arguments. *)

  (** {3 Retrieving input and output files} *)

  val infile_raw : _ t -> string option
  (** [infile_raw args] gets the input file as provided as an argument, if
      one indeed was. *)

  val infile_fpath : _ t -> Fpath.t option Or_error.t
  (** [infile_fpath args] behaves as {{!infile_raw} infile_raw}, but tries
      to parse any given input file as an Fpath. This may fail if the path
      is ill-formed. *)

  val infile_source : _ t -> Plumbing.Input.t Or_error.t
  (** [infile_source args] behaves as {{!infile_raw} infile_raw}, but tries
      to convert the result to an {{!Io.In_source.t} In_source.t}. This may
      fail if the path is ill-formed. *)

  val outfile_raw : _ t -> string option
  (** [outfile_raw args] gets the output file as provided as an argument, if
      one indeed was. *)

  val outfile_fpath : _ t -> Fpath.t option Or_error.t
  (** [outfile_fpath args] behaves as {{!outfile_raw} outfile_raw}, but
      tries to parse any given output file as an Fpath. This may fail if the
      path is ill-formed. *)

  val outfile_sink : _ t -> Plumbing.Output.t Or_error.t
  (** [outfile_sink args] behaves as {{!outfile_raw} outfile_raw}, but tries
      to convert the result to an {{!Io.Out_sink.t} Out_sink.t}. This may
      fail if the path is ill-formed. *)

  (** {3 Running filters using the given arguments} *)
  val run_filter :
       (module Plumbing.Filter_types.S
          with type aux_i = 'i
           and type aux_o = 'o)
    -> _ t
    -> aux_in:'i
    -> 'o Or_error.t
  (** [run_filter f args ~aux_in] runs the filter [f] with the file input
      and output arguments specified in [args], and the auxiliary input
      [aux_in], returning the auxiliary output or errors arising. *)

  val run_filter_with_aux_out :
       ?aux_out_filename:string
    -> (module Plumbing.Filter_types.S
          with type aux_i = 'i
           and type aux_o = 'o)
    -> _ t
    -> aux_in:'i
    -> aux_out_f:('o -> Stdio.Out_channel.t -> unit Or_error.t)
    -> unit Or_error.t
  (** [run_filter_with_aux_out ?aux_out_filename f args ~aux_in ~aux_out_f]
      runs the filter [f] with the file input and output arguments specified
      in [args], and the auxiliary input [aux_in]; it then outputs the
      auxiliary output to the file named by [aux_out_filename], using
      [aux_out_f], or otherwise discards it. It returns any errors arising. *)
end

(** {2 Miscellaneous argument helpers} *)

val id_type : Act_common.Id.t Command.Arg_type.t
(** [id_type] is an argument type for ACT IDs. *)

val asm_target : Asm_target.t Command.Param.t
(** [asm_target] defines a parameter for collecting an assembly target
    (architecture or compiler). *)

val flag_to_enum_choice :
  'a -> string -> doc:string -> 'a option Command.Param.t
(** [flag_to_enum_choice enum str ~doc] is a helper for implementing
    choose-one choices between multiple flags where each flag [str]
    corresponds to an enum variant [enum]. *)

val simulator :
  ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [simulator ?name ?doc ()] produces a parameter, normally named
    [-simulator] but overridable by [name], that accepts a simulator ID. *)

val arch :
  ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [arch ?name ?doc ()] produces a parameter, normally named [-arch] but
    overridable by [name], that accepts an architecture ID. *)

val aux_file : string option Command.Param.t
(** [aux_file] defines a parameter for receiving the path of an input litmus
    aux file. *)

val sanitiser_passes :
  Act_sanitiser.Pass_group.Selector.t Blang.t option Command.Param.t
(** [sanitiser_passes] defines a parameter for collecting a selector
    predicate for sanitiser passes. *)

val compiler_predicate :
  Act_compiler.Property.t Blang.t option Command.Param.t
(** [compiler_predicate] defines a parameter for collecting a filtering
    predicate for compilers. *)

val machine_predicate :
  Act_machine.Property.t Blang.t option Command.Param.t
(** [machine_predicate] defines a parameter for collecting a filtering
    predicate for machines. *)
