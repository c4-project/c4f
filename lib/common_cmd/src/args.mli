(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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
  (** [is_verbose t] gets whether, according to [t], verbose mode is switched
      on. *)

  val are_warnings_enabled : t -> bool
  (** [are_warnings_enabled t] gets whether, according to [t], warnings are
      switched on. *)

  val colour : t -> Fmt.style_renderer option
  (** [colour t] gets the TTY colouring mode, if any, according to [t]. *)

  val config_file : t -> string
  (** [config_file t] gets the configuration file according to [t]. *)
end

(** Wrapper of {!Standard} (etc) including arguments for (optional) input and
    output files. This is useful for exposing a filter as an act command.

    This module contains support for dealing with both single-input,
    single-output commands such as filters, and multi-input, single-output
    commands. *)
module With_files : sig
  type 'a t
  (** Opaque type of processed argument records. *)

  val rest : 'a t -> 'a
  (** [rest args] retrieves the argument record wrapped by [args]. *)

  (** {3 Parsing from the command line} *)

  val get : 'a Command.Param.t -> 'a t Command.Param.t
  (** [get cmd] lifts a [Command.Param.t] [cmd] over arguments to one that
      also retrieves the file arguments for a single-input, single-out. *)

  val get_with_multiple_inputs : 'a Command.Param.t -> 'a t Command.Param.t
  (** [get_with_multiple_inputs cmd] behaves as [get cmd], but allows for
      more than one input to be specified. *)

  (** {3 Retrieving input and output files} *)

  (** {4 Single input}

      These should be used with {!get}. They will fail with an error if more
      than one input argument was given. *)

  val infile_raw : _ t -> string option Or_error.t
  (** [infile_raw args] gets the input file provided as an argument, if one
      indeed was. It returns an error if more than one file was given. *)

  val infile_fpath : _ t -> Fpath.t option Or_error.t
  (** [infile_fpath args] behaves as {!infile_raw}, but tries to parse any
      given input file as an Fpath. This may fail if the path is ill-formed. *)

  val infile_source : _ t -> Plumbing.Input.t Or_error.t
  (** [infile_source args] behaves as {!infile_raw}, but tries to convert the
      result to an {!Plumbing.Input.t}. This may fail if the path is
      ill-formed. *)

  (** {4 Multiple input}

      These should be used with {!get_multiple_inputs}. *)

  val infiles_raw : _ t -> string list
  (** [infiles_raw args] gets the input files provided as an argument, if any
      indeed were. *)

  val infiles_fpath : _ t -> Fpath.t list Or_error.t
  (** [infiles_fpath args] behaves as {!infiles_raw}, but tries to parse any
      given input files as Fpaths. This may fail if any path is ill-formed. *)

  (** {4 Output} *)

  val outfile_raw : _ t -> string option
  (** [outfile_raw args] gets the output file as provided as an argument, if
      one indeed was. *)

  val outfile_fpath : _ t -> Fpath.t option Or_error.t
  (** [outfile_fpath args] behaves as {!outfile_raw}, but tries to parse any
      given output file as an Fpath. This may fail if the path is ill-formed. *)

  val outfile_sink : _ t -> Plumbing.Output.t Or_error.t
  (** [outfile_sink args] behaves as {!outfile_raw}, but tries to convert the
      result to an {!Io.Out_sink.t}. This may fail if the path is ill-formed. *)

  (** {3 Running filters using the given arguments} *)
  val run_filter :
       (module Plumbing.Filter_types.S
          with type aux_i = 'i
           and type aux_o = 'o)
    -> _ t
    -> aux_in:'i
    -> 'o Or_error.t
  (** [run_filter f args ~aux_in] runs the filter [f] with the file input and
      output arguments specified in [args], and the auxiliary input [aux_in],
      returning the auxiliary output or errors arising. *)

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

val backend :
  ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [backend ?name ?doc ()] produces a parameter, normally named [-backend]
    but overridable by [name], that accepts a backend ID. *)

val arch : ?name:string -> ?doc:string -> unit -> Id.t option Command.Param.t
(** [arch ?name ?doc ()] produces a parameter, normally named [-arch] but
    overridable by [name], that accepts an architecture ID. *)

val backend_arch : Act_backend.Arch.t option Command.Param.t
(** [backend_arch] is a parameter that accepts an architecture specifier for
    a backend. *)

val aux_file : string option Command.Param.t
(** [aux_file] defines a parameter for receiving the path of an input litmus
    aux file. *)

val sanitiser_passes :
  Act_sanitiser.Pass_group.Selector.t Blang.t option Command.Param.t
(** [sanitiser_passes] defines a parameter for collecting a selector
    predicate for sanitiser passes. *)

val backend_predicate : Act_backend.Property.t Blang.t option Command.Param.t
(** [backend_predicate] defines a parameter for collecting a filtering
    predicate for backends. *)

val compiler_predicate :
  Act_compiler.Property.t Blang.t option Command.Param.t
(** [compiler_predicate] defines a parameter for collecting a filtering
    predicate for compilers. *)

val machine_predicate : Act_machine.Property.t Blang.t option Command.Param.t
(** [machine_predicate] defines a parameter for collecting a filtering
    predicate for machines. *)
