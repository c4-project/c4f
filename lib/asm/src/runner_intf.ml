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
open Act_utils

(** [Basic] is a signature bringing together the modules we need to be able
    to run single-file jobs. *)
module type Basic = sig
  type ast

  (** [Src_lang] is the main language used in the jobs, which may differ
      from the [Litmus] language. *)
  module Src_lang : Act_language.Definition.S

  (** [Dst_lang] is the language used in emitted Litmus tests. *)
  module Dst_lang : Act_language.Definition.S

  module Frontend : Frontend.S with type ast := ast

  module Litmus_ast :
    Act_litmus.Ast.S
    with type Lang.Program.t = Dst_lang.Program.t
     and type Lang.Constant.t = Dst_lang.Constant.t

  module Litmus_pp : Act_litmus.Pp.S with module Ast = Litmus_ast

  module Multi_sanitiser :
    Act_sanitiser.Instance.S
    with module Lang := Src_lang
     and type 'a Program_container.t = 'a list

  module Single_sanitiser :
    Act_sanitiser.Instance.S
    with module Lang := Src_lang
     and type 'a Program_container.t = 'a

  val convert_program : Src_lang.Program.t -> Dst_lang.Program.t

  val convert_const : Src_lang.Constant.t -> Dst_lang.Constant.t Or_error.t

  val program : ast -> Src_lang.Program.t
end

module type Runnable = sig
  type program

  type cfg

  type sym

  val name : string

  val tmp_file_ext : string

  val default_config : unit -> cfg

  val run :
       Io.Out_sink.t
    -> Stdio.Out_channel.t
    -> in_name:string
    -> program:program
    -> symbols:sym list
    -> config:cfg
    -> passes:Act_config.Sanitiser_pass.Set.t
    -> Job.Output.t Or_error.t
end

(** Signature of job runners. *)
module type S = sig
  module Basic : Basic

  val make_output :
       string
    -> (string, string) List.Assoc.t
    -> Basic.Multi_sanitiser.Warn.t list
    -> Job.Output.t
  (** [make_output name symbols warnings] constructs a uniform job output. *)

  (** Uses this runner to construct a filter that performs a specified job
      on a single assembly file. *)
  module Make_filter
      (R : Runnable
           with type program := Basic.Src_lang.Program.t
            and type sym := Basic.Src_lang.Symbol.t) :
    Filter.S with type aux_i = R.cfg Job.t and type aux_o = Job.Output.t
end
