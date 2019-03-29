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

open Core_kernel

(* TODO(@MattWindsor91): lexer hack *)

module Normal = Utils.Frontend.Make (struct
  type ast = Ast.Translation_unit.t

  module I = Parser.MenhirInterpreter

  let lex = Lexer.token String.Set.empty

  let parse = Parser.Incremental.translation_unit

  let message = C_messages.message
end)

let litmus_predefined_types = String.Set.of_list ["atomic_int"]

module Litmus = Utils.Frontend.Make (struct
  type ast = Ast.Litmus.t

  module I = Parser.MenhirInterpreter

  let lex = Lexer.token litmus_predefined_types

  let parse = Parser.Incremental.litmus

  let message = C_messages.message
end)
