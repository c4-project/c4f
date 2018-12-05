(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Tester] provides the main `act` compiler tester, as used in the
    `act test` subcommand.

    The modules described in [Tester] run one or more compilers,
    across one or more machines, on a set of Memalloy-style C
    test cases with corresponding litmus files.  They then convert
    the resulting assembly into a litmus test, and (optionally)
    run Herd on the two tests, comparing the state sets emitted.

    These modules take the actual components used to do the testing---
    compilers, assembly job runners, and various other pieces of
    configuration---as parameters.  In `act`, most of these are
    filled in at the top-level, ie [Bin]. *)

open Base
open Utils

(** [Basic] is the signature common to both
    [Basic_machine] and [Basic_compiler]. *)
module type Basic = sig
  module T : Timing.S

  (** [o] tells the tester how to output warnings, errors, and
      other information. *)
  val o : Output.t

  (** [herd_opt], if present, tells the tester how to run Herd. *)
  val herd_opt : Herd.t option

  (** [sanitiser_passes] is the set of sanitiser passes the tester
      should use. *)
  val sanitiser_passes : Sanitiser_pass.Set.t
end

(** [Basic_compiler] contains all the various modules and components
    needed to run tests on one compiler. *)
module type Basic_compiler = sig
  include Basic

  (** [C] is the compiler interface for this compiler. *)
  module C : Compiler.S

  (** [R] is a runner for performing tasks on the assembly generated
      by [C]. *)
  module R : Asm_job.Runner

  (** [ps] tells the tester where it can find input files, and where
      it should put output files, for this compiler. *)
  val ps : Pathset.t

  (** [Basic_compiler] instances must provide a compiler spec and ID. *)
  include Compiler.With_spec
end

(** [Compiler] is the user-facing interface for running compiler tests
    on a single compiler. *)
module type Compiler = sig
  (** [run c_fnames] runs tests on each filename in [c_fnames],
      returning a compiler-level analysis. *)
  val run : string list -> Analysis.Compiler.t Or_error.t
end

(** [Make_compiler] makes a single-compiler test runner from a
   [Basic_compiler]. *)
module Make_compiler (B : Basic_compiler) : Compiler

(** [Basic_machine] contains all the various modules and components
    needed to run tests on one machine. *)
module type Basic_machine = sig
  include Basic

  (** [compiler_from_spec cspec] tries to get a [Compiler.S]
      corresponding to [cspec]. *)
  val compiler_from_spec
    :  Compiler.Spec.With_id.t
    -> (module Compiler.S) Or_error.t
  ;;

  (** [asm_runner_from_spec cspec] tries to get an [Asm_job.Runner]
      corresponding to [cspec]'s target architecture. *)
  val asm_runner_from_spec
    :  Compiler.Spec.With_id.t
    -> (module Asm_job.Runner) Or_error.t
  ;;
end

module type Machine = sig
  (** [run c_fnames specs ~in_root ~out_root] runs tests on each
     filename in [c_fnames], using every compiler in [specs] (presumed
     to belong to the same machine), reading from directories in
     [in_root] and writing to directories in [out_root], and returning
     a machine-level analysis. *)
  val run
    :  string list
    -> Compiler.Spec.Set.t
    -> in_root:string
    -> out_root:string
    -> Analysis.Machine.t Or_error.t
  ;;
end

(** [Make_machine] makes a single-machine test runner from a
   [Basic_machine]. *)
module Make_machine (B : Basic_machine) : Machine
