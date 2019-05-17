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

(** [S] is an interface for language-specific hooks into the sanitisation
    process. *)
module type S = sig
  include Pass.Basic

  module On_all :
    Pass.S
    with type t := Lang.Program.t Program_container.t
     and type 'a ctx := 'a Ctx.t

  module On_program :
    Pass.S with type t := Lang.Program.t and type 'a ctx := 'a Ctx.t

  module On_statement :
    Pass.S with type t := Lang.Statement.t and type 'a ctx := 'a Ctx.t

  module On_instruction :
    Pass.S with type t := Lang.Instruction.t and type 'a ctx := 'a Ctx.t

  module On_location :
    Pass.S with type t := Lang.Location.t and type 'a ctx := 'a Ctx.t
end

(** [S_maker] is the type of functors that generate hooks given a program
    container. *)
module type S_maker = functor (P : Travesty.Traversable.S1) -> S
                                                               with module Program_container = P
