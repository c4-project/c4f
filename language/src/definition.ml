(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

include Definition_intf

module Make (B : Basic) :
  S
  with type Symbol.t = B.Symbol.t
   and type Constant.t = B.Constant.t
   and type Location.t = B.Location.t
   and type Instruction.t = B.Instruction.t
   and type Statement.t = B.Statement.t
   and type Program.t = B.Program.t = struct
  include (B : Basic_core)

  module Symbol = Symbol.Make (B.Symbol)
  module Constant = Constant.Make (B.Constant)

  module Location = Location.Make (struct
    module Symbol = Symbol
    include B.Location
  end)

  module Instruction = Instruction.Make (struct
    module Constant = Constant
    module Symbol = Symbol
    module Location = Location
    include B.Instruction
  end)

  module Statement = Statement.Make (struct
    module Symbol = Symbol
    module Instruction = Instruction
    include B.Statement
  end)

  module Program = Program.Make (struct
    module Statement = Statement
    include B.Program
  end)
end
