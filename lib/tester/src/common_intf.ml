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

(** Signatures common to multiple different parts of the tester. *)

open Base
open Act_common
open Act_utils

(** [Basic] is the signature common to most tester [Basic] signatures. *)
module type Basic = sig
  (** The module to use for timing the various tester passes. *)
  module T : Timing.S

  val o : Output.t
  (** [o] tells the tester how to output warnings, errors, and other
      information. *)

  module C_simulator : Act_sim.Runner_intf.S

  val sanitiser_passes : Set.M(Act_sanitiser.Pass_group).t
  (** [sanitiser_passes] is the set of sanitiser passes the tester should
      use. *)
end

(** [Basic_machine_and_up] is the signature common to machine and instance
    level [Basic] signatures. *)
module type Basic_machine_and_up = sig
  include Basic

  (** Module used to resolve compiler specs to compiler modules. *)
  module Resolve_compiler :
    Act_compiler.Resolver.S
    with type spec = Act_compiler.Machine_spec.Qualified_compiler.t

  val asm_runner_from_spec :
       Act_compiler.Machine_spec.Qualified_compiler.t
    -> (module Act_asm.Runner_intf.Basic) Or_error.t
  (** [asm_runner_from_spec cspec] tries to get an [Asm_job.Runner]
      corresponding to [cspec]'s target architecture. *)
end

(** [Extensions] contains various extensions to [Basic] commonly imported
    into tester modules. *)
module type Extensions = sig
  type 'a timed

  module TS : Timing_set.S with type 'a input := 'a timed

  val bracket :
       ?id:Id.t
    -> ?machine:Id.t
    -> ?in_file:string
    -> ?out_file:string
    -> ?sub_stage:string
    -> (unit -> 'a Or_error.t)
    -> stage:string
    -> 'a timed Or_error.t
  (** [bracket ?id ?machine ?in_file ?out_file ?sub_stage f ~stage] runs
      [f], using the timing wrapper [timed] and logging the tester stage
      [stage] with the various logging options passed through. *)
end
