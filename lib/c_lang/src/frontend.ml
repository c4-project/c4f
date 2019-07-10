(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): lexer hack *)

module Normal = Act_utils.Frontend.Make (struct
  type ast = Ast.Translation_unit.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token =
    Lexer.token (Set.empty (module String))

  let parse = Parser.Incremental.translation_unit

  let message = C_messages.message
end)

let litmus_predefined_types = Set.of_list (module String) ["atomic_int"]

module Litmus = Act_utils.Frontend.Make (struct
  type ast = Ast.Litmus.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token =
    Lexer.token litmus_predefined_types

  let parse = Parser.Incremental.litmus

  let message = C_messages.message
end)

module Litmus_post = Act_utils.Frontend.Make (struct
  type ast = Ast_basic.Constant.t Act_litmus.Postcondition.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token =
    Lexer.token litmus_predefined_types

  let parse = Parser.Incremental.litmus_postcondition

  let message = C_messages.message
end)
