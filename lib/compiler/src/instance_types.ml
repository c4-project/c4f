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

open Base

(** [Basic] is the basic interface compilers must implement. *)
module type Basic = sig
  val test_args : string list
  (** [test_args] is the set of arguments sent to the compiler to test that
      it works. Usually, this will be some form of version command. *)

  val compile_args :
       args:string list
    -> emits:Act_common.Id.t
    -> infile:string
    -> outfile:string
    -> string list
  (** [compile_args ~args ~emits ~infile ~outfile] takes the set of
      arguments [args] the user supplied, the name of the architecture
      [emits] the compiler is going to emit, the input file [infile], and
      the output file [outfile], and produces a final argument vector to
      send to the compiler. *)
end

(** [S] is the outward-facing interface of compiler modules. *)
module type S = sig
  val test : unit -> unit Or_error.t
  (** [test ()] tests that the compiler is working. *)

  val compile : infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t
  (** [compile ~infile ~outfile] runs the compiler on [infile], emitting
      assembly to [outfile] and returning any errors that arise. *)
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
