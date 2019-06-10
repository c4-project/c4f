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
open Act_common
open Parser
module Tx = Travesty_core_kernel_exts
module S = Sedlexing
module F = Act_utils.Frontend
module Lu = Act_utils.Lex_utils
module String_map = Map.M (String.Caseless)

let keywords : token String_map.t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module String.Caseless)
       [ ("action", ACTION)
       ; ("arch", ARCH)
       ; ("argv", ARGV)
       ; ("asm_model", ASM_MODEL)
       ; ("c_model", C_MODEL)
       ; ("cmd", CMD)
       ; ("cpp", CPP)
       ; ("compiler", COMPILER)
       ; ("copy", COPY)
       ; ("default", DEFAULT)
       ; ("enabled", ENABLED)
       ; ("false", BOOL false)
       ; ("fuzz", FUZZ)
       ; ("host", HOST)
       ; ("local", LOCAL)
       ; ("machine", MACHINE)
       ; ("no", BOOL false)
       ; ("on", BOOL true)
       ; ("off", BOOL false)
       ; ("sim", SIM)
       ; ("ssh", SSH)
       ; ("style", STYLE)
       ; ("to", TO)
       ; ("true", BOOL true)
       ; ("try", TRY)
       ; ("user", USER)
       ; ("via", VIA)
       ; ("weight", WEIGHT)
       ; ("yes", BOOL true) ])

let digit = [%sedlex.regexp? '0' .. '9']

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let num = [%sedlex.regexp? Plus digit]

let eol = [%sedlex.regexp? '\n']

let c_id_start = [%sedlex.regexp? alpha | '_']

let c_id_middle = [%sedlex.regexp? c_id_start | digit | '.']

let c_id = [%sedlex.regexp? c_id_start, Star c_id_middle]

let space = [%sedlex.regexp? Sub (white_space, '\n')]

let id_token (lexeme : string) : token = IDENTIFIER (Id.of_string lexeme)

let lex_possible_keyword (lexeme : string) : token =
  lexeme
  |> Map.find (force keywords)
  |> Tx.Option.value_f ~default_f:(fun () -> id_token lexeme)

let rec token (lexbuf : Sedlexing.lexbuf) : token =
  match%sedlex lexbuf with
  | space ->
      token lexbuf
  | eof ->
      EOF
  | eol ->
      EOL
  | '#' ->
      Lu.skip_line lexbuf ; EOL
  | '{' ->
      LBRACE
  | '}' ->
      RBRACE
  | '"' ->
      Lu.read_string (fun x -> STRING x) lexbuf
  | Opt '-', num ->
      INTEGER (Int.of_string (S.Utf8.lexeme lexbuf))
  | c_id ->
      lex_possible_keyword (S.Utf8.lexeme lexbuf)
  | _ ->
      F.lex_error ("Unexpected char: " ^ S.Utf8.lexeme lexbuf) lexbuf
