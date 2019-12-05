(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for {!Instance}. *)

open Base

(** {1 Input} *)

(** Signature of input to {!Instance.Make}. *)
module type Basic = sig
  val test_args : string list
  (** [test_args] is the set of arguments sent to the backend to test that it
      works. Usually, this will be some sort of version command. *)

  val capabilities : test_stdout:string list -> Capability.Summary.t
  (** [capabilities ~test_stdout] gets a broad set of capabilities that this
      backend has, given the output of running the backend with [test_args]. *)

  module Reader : Reader_types.S
  (** The reader module for this backend. *)

  val run : Spec.t -> arch:Arch.t -> Capability.Run.t
  (** [run spec ~arch] asks this backend if it can run directly as a filter
      with architecture [arch].

      It returns either a description of how to do so (as a function from
      input files to argument vectors), or an explanation as to why not. *)

  val make_harness : Spec.t -> arch:Arch.t -> Capability.Make_harness.t
  (** [make_harness spec ~arch] asks this backend if it can run directly as a
      filter from input path [input_path] to an observation on stdout.

      It returns either a description of how to do so (as a function from
      input files and output directories to argument vectors), or an
      explanation as to why not. *)
end

(** {1 Output} *)

(** Interface for runnable instances of a backend. *)
module type S = sig
  module Reader : Reader_types.S
  (** Allows reading in this backend's output. *)

  module Filter : Filter.S
  (** Allows running the backend as a filter. *)

  val make_harness :
       Arch.t
    -> input_file:Fpath.t
    -> output_dir:Fpath.t
    -> string list Or_error.t
  (** [make_harness ctx ~input_file ~output_dir], on backends that support
      it, constructs an executable environment in the output directory
      [output_dir], and returns a list of shell commands that, when run
      inside that directory, perform a test on [input_path] and return the
      results as a file that can be read using [read]. *)

  val run :
       Arch.t
    -> input_file:Fpath.t
    -> output_file:Fpath.t
    -> Act_state.Observation.t Or_error.t
  (** [run ctx ~input_file ~output_path] runs the backend on [input_file],
      using [ctx] as context, outputs to [output_file], and then tries to
      load back the results as a generic observation. *)
end

(** [Basic_with_run_info] is a signature collecting both a base backend
    specification and context about how to run the backend. *)
module type Basic_with_run_info = sig
  include Basic

  val spec : Spec.t Act_common.Spec.With_id.t
  (** The full name-qualified backend spec. *)

  module Runner : Plumbing.Runner_types.S
  (** The runner on which the backend will run. *)
end
