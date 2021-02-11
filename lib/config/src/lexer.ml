(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open C4f_common
open Parser
module Tx = Travesty_base_exts
module S = Sedlexing
module F = C4f_utils.Frontend
module Lu = C4f_utils.Lex_utils
module String_map = Map.M (String.Caseless)

let keywords : token String_map.t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module String.Caseless)
       [ ("action", ACTION)
       ; ("false", BOOL false)
       ; ("flag", FLAG)
       ; ("fuzz", FUZZ)
       ; ("no", BOOL false)
       ; ("on", BOOL true)
       ; ("off", BOOL false)
       ; ("param", PARAM)
       ; ("ratio", RATIO)
       ; ("set", SET)
       ; ("to", TO)
       ; ("true", BOOL true)
       ; ("weight", WEIGHT)
       ; ("yes", BOOL true) ] )

let digit = [%sedlex.regexp? '0' .. '9']

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let num = [%sedlex.regexp? Plus digit]

let eol = [%sedlex.regexp? '\n']

let id_start = [%sedlex.regexp? alpha | '_']

let id_middle = [%sedlex.regexp? id_start | digit | '-' | '.']

let id = [%sedlex.regexp? id_start, Star id_middle]

let space = [%sedlex.regexp? Sub (white_space, '\n')]

let id_token (lexeme : string) : token = IDENTIFIER (Id.of_string lexeme)

let lex_possible_keyword (lexeme : string) : token =
  lexeme
  |> Map.find (force keywords)
  |> Tx.Option.value_f ~default_f:(fun () -> id_token lexeme)

let rec token (lexbuf : Sedlexing.lexbuf) : token =
  match%sedlex lexbuf with
  | space -> token lexbuf
  | eof -> EOF
  | eol -> EOL
  | '#' -> Lu.skip_line lexbuf ; EOL
  | '{' -> LBRACE
  | '}' -> RBRACE
  | ':' -> COLON
  | Opt '-', num -> INTEGER (Int.of_string (S.Utf8.lexeme lexbuf))
  | id -> lex_possible_keyword (S.Utf8.lexeme lexbuf)
  | _ -> F.lex_error ("Unexpected char: " ^ S.Utf8.lexeme lexbuf) lexbuf
