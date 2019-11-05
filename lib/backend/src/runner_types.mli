(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Backend runner interfaces.

    This module provides a standard interface for interacting with C and
    assembly backends, such as Herd and Litmus, as well as a functor for
    implementing that interface using a filter wrapper over the simulator and
    a simulator output parser. *)

open Base

(** {3 Input interfaces} *)

(** Usual input for simulator runner functors. *)
module type Basic = sig
  val machine_id : Act_common.Id.t
  (** The ID of the machine running this simulator. *)

  val spec : Spec.t
  (** The simulator spec describing how to run this simulator. *)

  module Runner : Plumbing.Runner_types.S
  (** Runner used for running the simulator, possibly remotely. *)
end

(** {3 Output interfaces} *)

(** Main interface for simulator runners. *)
module type S = sig
  module Reader : Reader_types.S
  (** Allows reading in this simulator's output. *)

  module Filter : Filter.S
  (** Allows running the simulator as a filter. *)

  val make_harness :
       Arch.t
    -> input_path:Fpath.t
    -> output_dir:Fpath.t
    -> string list Or_error.t
  (** [make_harness ctx ~input_path ~output_dir], on backends that support
      it, constructs an executable environment in the output directory
      [output_dir], and returns a list of shell commands that, when run
      inside that directory, perform a test on [input_path] and return the
      results as a file that can be read using [read]. *)

  val run :
       Arch.t
    -> input_path:Fpath.t
    -> output_path:Fpath.t
    -> Act_state.Observation.t Or_error.t
  (** [run ctx ~input_path ~output_path] runs the backend on [input_path],
      using [ctx] as context, outputs to [output_path], and then tries to
      load back the results as a generic observation. *)
end
