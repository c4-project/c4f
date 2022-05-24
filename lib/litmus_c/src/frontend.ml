(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): lexer hack *)

module Normal = C4f_utils.Frontend.Make (struct
  type ast = Ast.Translation_unit.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token =
    Lexer.token (Set.empty (module String))

  let parse = Parser.Incremental.translation_unit

  let message = Messages.message
end)

let litmus_predefined_types =
  Set.of_list (module String) ["atomic_bool"; "atomic_int"; "bool"]

module Litmus = C4f_utils.Frontend.Make (struct
  type ast =
    (Ast.Litmus_lang.Constant.t, Ast.Litmus_lang.Program.t) C4f_litmus.Ast.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token = Lexer.token litmus_predefined_types

  let parse = Parser.Incremental.litmus

  let message = Messages.message
end)

module Litmus_post = C4f_utils.Frontend.Make (struct
  type ast = Ast_basic.Constant.t C4f_litmus.Postcondition.t

  module I = Parser.MenhirInterpreter

  let lex : Sedlexing.lexbuf -> I.token = Lexer.token litmus_predefined_types

  let parse = Parser.Incremental.litmus_postcondition

  let message = Messages.message
end)

module Fir =
  Plumbing.Loadable.Make_chain
    (Litmus)
    (struct
      type dst = C4f_fir.Litmus.Test.t

      let f = Abstract.litmus_of_raw_ast
    end)
