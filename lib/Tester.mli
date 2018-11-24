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

(** [Basic_machine] contains all the various modules and components
    needed to run tests on one machine. *)
module type Basic_machine = sig
  module T : Timing.S

  (** [o] tells the tester how to output warnings, errors, and
      other information. *)
  val o : Output.t

  (** [ps] tells the tester where it can find input files, and where
      it should put output files. *)
  val ps : Pathset.t

  (** [herd_opt], if present, tells the tester how to run Herd. *)
  val herd_opt : Herd.t option
end

(** [Basic_compiler] contains all the various modules and components
    needed to run tests on one compiler. *)
module type Basic_compiler = sig
  include Basic_machine

  (** [C] is the compiler interface for this compiler. *)
  module C : Compiler.S
  (** [R] is a runner for performing tasks on the assembly generated
      by [C]. *)
  module R : Asm_job.Runner

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
