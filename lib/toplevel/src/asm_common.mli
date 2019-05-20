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

    This module sits on top of {{!Common}Common}. *)

open Base

module Input : sig
  type t
  (** Opaque type of assembly command input. *)

  val act_config : t -> Act_config.Act.t
  val file_type : t -> Act_config.File_type.t_or_infer
  val infile_raw : t -> string option
  val outfile_raw : t -> string option
  val output : t -> Act_common.Output.t
  val sanitiser_passes : t -> Act_config.Sanitiser_pass.Set.t
  val target : t -> Act_config.Compiler.Target.t
  val user_cvars : t -> Act_common.C_variables.Map.t option

  val make_compiler_input :
    t
    -> (c_variables:Act_common.C_variables.Map.t option -> 'cfg)
    -> Act_c.Filters.Output.t Act_utils.Filter.chain_output
    -> 'cfg Act_asm.Job.t Act_config.Compiler.Chain_input.t
end


val lift_command
  : Args.Standard_asm.t ->
  f:(Input.t -> unit Or_error.t) ->
  default_passes:Act_config.Sanitiser_pass.Set.t ->
  unit
