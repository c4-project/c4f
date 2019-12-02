(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** [Basic] is the basic interface compilers must implement. *)
module type Basic = sig
  val test_args : string list
  (** [test_args] is the set of arguments sent to the compiler to test that
      it works. Usually, this will be some form of version command. *)

  val compile_args :
       user_args:string list
    -> arch:Act_common.Id.t
    -> mode:Mode.t
    -> infiles:string list
    -> outfile:string
    -> string list
  (** [compile_args ~user_args ~arch ~mode ~infiles ~outfile] takes the set
      of arguments [args] the user supplied, the name of the architecture
      [arch] the compiler is going to emit, information about the compilation
      mode [mode], and input [infiles] and output [outfile] files; it then
      produces a final argument vector to send to the compiler. *)
end

(** [S] is the outward-facing interface of compiler modules. *)
module type S = sig
  val test : unit -> unit Or_error.t
  (** [test ()] tests that the compiler is working. *)

  val compile :
    Mode.t -> infiles:Fpath.t list -> outfile:Fpath.t -> unit Or_error.t
  (** [compile mode ~infiles ~outfile] runs the compiler on [infiles],
      emitting the output specified by [mode] to [outfile] and returning any
      errors that arise. *)
end

(** [With_spec] is an interface for modules containing a (full) compiler
    specification. *)
module type With_spec = sig
  val cspec : Spec.With_id.t
end

(** [Basic_with_run_info] is a signature collecting both a base compiler
    specification and context about how to run the compiler. *)
module type Basic_with_run_info = sig
  include Basic

  include With_spec

  module Runner : Plumbing.Runner_types.S
end
