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
  (** Opaque type of processed argument records. *)
  type t

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

  val load_config : t -> Act_config.Global.t Or_error.t
  (** [load_config t] loads the config at [config_file t]. *)

  (** {2 Lifting commands over standard arguments} *)

  val lift_command : t -> f:(Output.t -> unit Or_error.t) -> unit
  (** [lift_command standard_args ~f] lifts a command body [f], performing
      common book-keeping such as colourising, creating an {!Output.t}, and
      printing top-level errors.

      It does not load the configuration file, even if one is given; use
      {!lift_command_with_config} instead. (The two functions are separate to
      allow for commands that need to operate without configuration present.) *)

  val lift_command_with_config :
    t -> f:(Output.t -> Act_config.Global.t -> unit Or_error.t) -> unit
  (** [lift_command_with_config standard_args ~f] acts like [lift_command],
      but also loads and tests the configuration. *)
end

(** Wrapper of {!Standard} (etc) including arguments for (optional) input and
    output files. This is useful for exposing a filter as an act command.

    This module contains support for dealing with both single-input,
    single-output commands such as filters, and multi-input, single-output
    commands. *)
module With_files : sig
  (** Opaque type of processed argument records. *)
  type 'a t

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
       ('i -> Plumbing.Input.t -> Plumbing.Output.t -> 'o Or_error.t)
    -> _ t
    -> aux_in:'i
    -> 'o Or_error.t
  (** [run_filter f args ~aux_in] runs the filter [f] with the file input and
      output arguments specified in [args], and the auxiliary input [aux_in],
      returning the auxiliary output or errors arising. *)
end

(** {2 Miscellaneous argument helpers} *)

val id_type : Act_common.Id.t Command.Arg_type.t
(** [id_type] is an argument type for ACT IDs. *)

val fpath_type : Fpath.t Command.Arg_type.t
(** [fpath_type] is an argument type for Fpaths. *)

val input_type : Plumbing.Input.t Command.Arg_type.t
(** [input_type] is an argument type for Plumbing inputs. For stdin, use the
    {!With_files} argument helpers instead. *)

val output_type : Plumbing.Output.t Command.Arg_type.t
(** [output_type] is an argument type for Plumbing outputs. For stdout, use
    the {!With_files} argument helpers instead. *)

val flag_to_enum_choice :
  'a -> string -> doc:string -> 'a option Command.Param.t
(** [flag_to_enum_choice enum str ~doc] is a helper for implementing
    choose-one choices between multiple flags where each flag [str]
    corresponds to an enum variant [enum]. *)

val aux_file : string option Command.Param.t
(** [aux_file] defines a parameter for receiving the path of an input litmus
    aux file. *)
