(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** Assembly sanitisation *)

open Base

(** [S] is the interface to a fully-built sanitiser. *)
module type S = sig
  module Lang : Act_language.Definition_types.S
  (** [Lang] is the language over which we are sanitising. *)

  module Warn :
    Warn_intf.S
      with type t = Lang.Element.t Warn.t
       and type elt = Lang.Element.t

  module Output :
    Output_intf.S
      with type listing := Lang.Program.t
       and type ('w, 'l) program := ('w, 'l) Output.Program.t
       and type warn_elt := Lang.Element.t
       and type rmap := Lang.Symbol.R_map.t

  val sanitise :
       ?passes:Set.M(Pass_group).t
    -> ?symbols:Lang.Symbol.t list
    -> Lang.Program.t
    -> Output.t Or_error.t
  (** [sanitise ?passes ?symbols stms] sanitises a statement list [stms],
      returning one or more program lists with various litmus-unfriendly
      elements removed or simplified.

      If the implementation of the sanitiser outputs multiple programs,
      [stms] will be split along its program boundaries first.

      Optional argument 'passes' controls the sanitisation passes used; the
      default is [Pass.all].

      Optional argument 'symbols' gives a list of concrete symbols that the
      sanitiser should track throughout the sanitisation process. *)
end
