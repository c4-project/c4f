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

(** Interfaces used in {{!Stub_gen} Stub_gen}. *)

open Base

(** [Basic] collects the input required to generate stubs. This should be a
    subset of {{!Runner.Basic} Runner.Basic}. *)
module type Basic = sig
  (** [Src_lang] is the language under explanation. *)
  module Src_lang : Act_language.Definition.S

  module Sanitiser_hook :
    Act_sanitiser.Hook_intf.S with module Lang = Src_lang

  module Program :
    Act_utils.Loadable_intf.S with type t = Src_lang.Program.t

  val as_asm_stub : Src_lang.Program.t -> Act_c.Asm_stub.t Or_error.t
  (** [as_asm_stub t ~oc] outputs a GCC assembly stub representation of this
      program onto [oc]. It can fail, for example if the language doesn't
      support such dumping. *)
end

module type S = sig
  (** Opaque type of stub generator config. *)
  type config

  module Lang : Act_language.Definition.S

  module Filter : Runner_intf.S with type cfg = config
end
