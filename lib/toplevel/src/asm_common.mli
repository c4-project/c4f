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
  (** Opaque type of assembly command input. *)
  type t

  val act_config : t -> Act_config.Act.t

  val file_type : t -> Act_common.File_type.t

  val pb_input : t -> Plumbing.Input.t

  val pb_output : t -> Plumbing.Output.t

  val output : t -> Act_common.Output.t

  val sanitiser_passes : t -> Set.M(Act_sanitiser.Pass_group).t

  val target : t -> Act_compiler.Instance.Target.t

  val user_cvars : t -> Act_common.C_variables.Map.t option

  val make_compiler_input :
       t
    -> (c_variables:Act_common.C_variables.Map.t option -> 'cfg)
    -> Act_c.Filters.Output.t Plumbing.Chain_context.t
    -> 'cfg Act_asm.Job.t Act_compiler.Instance.Chain_input.t
end

val lift_command :
     Args.Standard_asm.t
  -> f:(Input.t -> unit Or_error.t)
  -> default_passes:Set.M(Act_sanitiser.Pass_group).t
  -> unit
