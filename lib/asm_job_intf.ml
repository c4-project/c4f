(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base
open Utils

(** Generalised signature of job runners. *)
module type Gen_runner = sig
  type 'fmt inp
  (** Type of input, parametrised over format enum. *)

  type aux
  (** Type of auxiliary output. *)

  type lcfg
  (** Type of Litmus-specific config. *)

  type ecfg
  (** Type of explainer-specific config. *)

  module Litmusify : Filter.S with type aux_i = lcfg inp
                               and type aux_o = aux
  (** [Litmusify] is a filter that runs a litmusifying job. *)

  module Explain : Filter.S with type aux_i = ecfg inp
                             and type aux_o = aux
  (** [Explain] is a filter that runs an assembly explanation job. *)
end

(** [Runner_deps] is a signature bringing together the modules we
    need to be able to run single-file jobs. *)
module type Runner_deps = sig
  type ast

  module Src_lang : Language.S
  (** [Src_lang] is the main language used in the jobs, which may differ
      from the [Litmus] language. *)

  module Dst_lang : Language.S
  (** [Dst_lang] is the language used in emitted Litmus tests. *)

  module Frontend : Frontend.S with type ast := ast

  module Litmus_ast : Litmus.Ast.S with type Lang.Program.t = Dst_lang.Program.t
                                    and type Lang.Constant.t = Dst_lang.Constant.t
  module Litmus_pp : Litmus.Pp.S with module Ast = Litmus_ast

  module Multi_sanitiser
    : Sanitiser.S with module Lang := Src_lang
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with module Lang := Src_lang
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer : Explainer.S with module Lang := Src_lang

  val convert_program
    :  Src_lang.Program.t
    -> Dst_lang.Program.t

  val convert_const
    :  Src_lang.Constant.t
    -> Dst_lang.Constant.t Or_error.t

  val program : ast -> Src_lang.Program.t
end
