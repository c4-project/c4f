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

(** [Basic] is a signature bringing together the modules we need to be able
    to run single-file jobs. *)
module type Basic = sig
  (** [Src_lang] is the main language used in the jobs, which may differ
      from the [Litmus] language. *)
  module Src_lang : Act_language.Definition.S

  (** [Dst_lang] is the language used in emitted Litmus tests. *)
  module Dst_lang : Act_language.Definition.S

  module Litmus_ast :
    Act_litmus.Ast.S
    with type Lang.Program.t = Dst_lang.Program.t
     and type Lang.Constant.t = Dst_lang.Constant.t

  module Litmus_pp : Act_litmus.Pp_intf.S with module Ast = Litmus_ast

  module Sanitiser_hook :
    Act_sanitiser.Hook_intf.S with module Lang = Src_lang

  val as_asm_stub : int -> Src_lang.Program.t -> Act_c.Asm_stub.t Or_error.t
  (** [as_asm_stub tid t ~oc] outputs a GCC assembly stub representation of
      program [t], with thread ID [tid], onto [oc]. It can fail, for example
      if the language doesn't support such dumping. *)

  val convert_program : Src_lang.Program.t -> Dst_lang.Program.t Or_error.t

  val convert_const : Src_lang.Constant.t -> Dst_lang.Constant.t Or_error.t

  module Program :
    Plumbing.Loadable_types.S with type t = Src_lang.Program.t
end

module type Runnable = sig
  module Program : Plumbing.Loadable_types.S

  type cfg

  module Symbol : sig
    type t

    val of_string_opt : string -> t option
  end

  val name : string

  val tmp_file_ext : string

  val default_config : unit -> cfg

  val run :
       Stdio.Out_channel.t
    -> in_name:string
    -> program:Program.t
    -> symbols:Symbol.t list
    -> config:cfg
    -> passes:Set.M(Act_sanitiser.Pass_group).t
    -> Job.Output.t Or_error.t
end

(** Signature of job runners. *)
module type S = sig
  (** Type of configuration *)
  type cfg

  include
    Plumbing.Filter_types.S
    with type aux_i = cfg Job.t
     and type aux_o = Job.Output.t
end
