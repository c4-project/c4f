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

open Core_kernel
open Lib

(** [Basic] contains all the various modules and components needed to run
    tests on one machine. *)
module type Basic = sig
  include Common_intf.Basic

  val compilers : Config.Compiler.Spec.Set.t
  (** [compilers] is the set of all enabled compilers for this machine. *)

  (** Module used to resolve compiler specs to compiler modules. *)
  module Resolve_compiler :
    Config.Compiler.S_resolver
    with type spec = Config.Compiler.Spec.With_id.t
     and type 'a chain_input = 'a Config.Compiler.Chain_input.t

  val asm_runner_from_spec :
    Config.Compiler.Spec.With_id.t -> (module Asm_job.Runner) Or_error.t
  (** [asm_runner_from_spec cspec] tries to get an [Asm_job.Runner]
      corresponding to [cspec]'s target architecture. *)
end

module type S = sig
  val run :
    Run_config.t -> Sim.Bulk.File_map.t -> Analysis.Machine.t Or_error.t
  (** [run cfg c_sims] runs tests on each filename listed in [cfg], using
      every machine-local compiler in [specs] also listed in [cfg], to
      belong to the same machine), reading from directories in [cfg]'s
      [in_root] and writing to directories in its [out_root], and returning
      a machine-level analysis. *)
end
