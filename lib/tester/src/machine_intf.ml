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

(** [Basic] contains all the various modules and components needed to run
    tests on one machine. *)
module type Basic = sig
  include Common_intf.Basic_machine_and_up

  val spec : Act_compiler.Machine_spec.With_id.t
  (** [spec] is the specification of this machine. *)

  val asm_simulators : Act_sim.Table.t
  (** The table of simulators available to this machine. *)
end

module type S = sig
  val run :
    Run_config.t -> Act_sim.Bulk.File_map.t -> Analysis.Machine.t Or_error.t
  (** [run cfg c_sims] runs tests on each filename listed in [cfg], using
      every machine-local compiler in [specs] also listed in [cfg], to
      belong to the same machine), reading from directories in [cfg]'s
      [in_root] and writing to directories in its [out_root], and returning
      a machine-level analysis. *)
end
