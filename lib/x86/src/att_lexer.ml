(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Att_parser
module S = Sedlexing
module F = Act_utils.Frontend
module Lu = Act_utils.Lex_utils

let digit = [%sedlex.regexp? '0' .. '9']

let hex = [%sedlex.regexp? ascii_hex_digit]

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let num = [%sedlex.regexp? Plus digit]

let ws = [%sedlex.regexp? Chars " \t\r"]

let eol = [%sedlex.regexp? '\n']

let lex_reg (lexbuf : S.lexbuf) : token =
  let name = String.chop_prefix_exn ~prefix:"%" (S.Utf8.lexeme lexbuf) in
  match Reg.of_string_option name with
  | Some r ->
      ATT_REG r
  | None ->
      F.lex_error ("Invalid register: " ^ name) lexbuf

(* Symbol lexing See:
   https://sourceware.org/binutils/docs/as/Symbol-Intro.html#Symbol-Intro *)
let nameprf = [%sedlex.regexp? alpha | '_' | '.']

let namechr = [%sedlex.regexp? nameprf | digit | '$' | '.']

let name = [%sedlex.regexp? nameprf, Star namechr]

let num = [%sedlex.regexp? Plus digit]

let hexnum = [%sedlex.regexp? Plus hex]

let rec token (lexbuf : S.lexbuf) : token =
  match%sedlex lexbuf with
  | ws ->
      token lexbuf
  | eol ->
      EOL
  | "/*" ->
      Lu.skip_c_comment lexbuf ; token lexbuf
  | '#' ->
      Lu.skip_line lexbuf ; EOL
  | Opt '-', num ->
      NUM (S.Utf8.lexeme lexbuf)
  | '$' ->
      DOLLAR
  | "0x", hexnum ->
      ATT_HEX (String.chop_prefix_exn ~prefix:"0x" (S.Utf8.lexeme lexbuf))
  | "0x" ->
      F.lex_error ("Malformed hex constant: " ^ S.Utf8.lexeme lexbuf) lexbuf
  | '%', name ->
      lex_reg lexbuf
  | ',' ->
      COMMA
  | '(' ->
      LPAR
  | ')' ->
      RPAR
  | ':' ->
      COLON
  | '+' ->
      PLUS
  | '-' ->
      MINUS
  | '"' ->
      Lu.read_string (fun x -> STRING x) lexbuf
  | "lock" ->
      IT_LOCK
  | name ->
      NAME (S.Utf8.lexeme lexbuf)
  | '@', name ->
      GAS_TYPE (S.Utf8.lexeme lexbuf)
  | eof ->
      EOF
  | _ ->
      F.lex_error ("Unexpected char: " ^ S.Utf8.lexeme lexbuf) lexbuf
