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

(** Common infrastructure for [act asm] commands.

    This module sits on top of {{!Common} Common}. *)

open Base

module Input : sig
  type t
  (** Opaque type of assembly command input. *)

  val act_config : t -> Act_config.Act.t

  val pb_input : t -> Plumbing.Input.t

  val pb_output : t -> Plumbing.Output.t

  val output : t -> Act_common.Output.t

  val sanitiser_passes : t -> Set.M(Act_sanitiser.Pass_group).t

  val target : t -> Act_machine.Target.t

  val c_litmus_aux : t -> Act_delitmus.Aux.t
  (** [c_litmus_aux in] gets the auxiliary information associated with any C
      litmus test that was previously processed into the input to this
      assembly command. *)

  val make_job_input :
    t -> (Act_delitmus.Aux.t -> 'cfg) -> 'cfg Act_asm.Job.t
end

val lift_command :
     Args.Standard_asm.t
  -> f:(Input.t -> unit Or_error.t)
  -> default_passes:Set.M(Act_sanitiser.Pass_group).t
  -> unit
