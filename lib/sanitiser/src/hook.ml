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

include Hook_intf

module Make_null
    (Lang : Act_language.Definition.S)
    (P : Travesty.Traversable.S1) :
  S with module Lang = Lang and module Program_container = P = struct
  module Lang = Lang
  module Ctx = Ctx.Make (Lang)
  module Program_container = P
  module Null = Pass.Make_null (Ctx)

  module On_all = Null (struct
    type t = Lang.Program.t Program_container.t
  end)

  module On_program = Null (Lang.Program)
  module On_statement = Null (Lang.Statement)
  module On_instruction = Null (Lang.Instruction)
  module On_location = Null (Lang.Location)
end
