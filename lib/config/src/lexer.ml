(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common
open Parser
module Tx = Travesty_base_exts
module S = Sedlexing
module F = Act_utils.Frontend
module Lu = Act_utils.Lex_utils
module String_map = Map.M (String.Caseless)

let keywords : token String_map.t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module String.Caseless)
       [ ("action", ACTION)
       ; ("argv", ARGV)
       ; ("asm_model", ASM_MODEL)
       ; ("backend", BACKEND)
       ; ("c_model", C_MODEL)
       ; ("cmd", CMD)
       ; ("copy", COPY)
       ; ("default", DEFAULT)
       ; ("enabled", ENABLED)
       ; ("false", BOOL false)
       ; ("flag", FLAG)
       ; ("fuzz", FUZZ)
       ; ("host", HOST)
       ; ("local", LOCAL)
       ; ("machine", MACHINE)
       ; ("no", BOOL false)
       ; ("on", BOOL true)
       ; ("off", BOOL false)
       ; ("param", PARAM)
       ; ("ratio", RATIO)
       ; ("set", SET)
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
  | ':' ->
      COLON
  | '"' ->
      Lu.read_string (fun x -> STRING x) lexbuf
  | Opt '-', num ->
      INTEGER (Int.of_string (S.Utf8.lexeme lexbuf))
  | c_id ->
      lex_possible_keyword (S.Utf8.lexeme lexbuf)
  | _ ->
      F.lex_error ("Unexpected char: " ^ S.Utf8.lexeme lexbuf) lexbuf
