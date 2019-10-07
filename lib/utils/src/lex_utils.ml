(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

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
   USE OR OTHER DEALINGS IN THE SOFTWARE.

   This file derives in part from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(* the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

open Base
module S = Sedlexing
module F = Frontend

let ml_com_e = [%sedlex.regexp? "*)"]

let c_com_e = [%sedlex.regexp? "*/"]

let c_com_l = [%sedlex.regexp? "//"]

let eol = [%sedlex.regexp? '\n']

let rec skip_ml_comment_inner (nesting : int) (lexbuf : S.lexbuf) : unit =
  match%sedlex lexbuf with
  | "(*" ->
      skip_ml_comment_inner (nesting + 1) lexbuf
  | '(' ->
      skip_ml_comment_inner nesting lexbuf
  | "*)" ->
      if 1 < nesting then skip_ml_comment_inner (nesting - 1) lexbuf
  | '*' ->
      skip_ml_comment_inner nesting lexbuf
  | eof ->
      F.lex_error "eof in skip_ml_comment" lexbuf
  | Plus (Compl '(' | '*' | eof) ->
      skip_ml_comment_inner nesting lexbuf
  | _ ->
      F.lex_error "failure in skip_ml_comment_inner" lexbuf

let skip_ml_comment : S.lexbuf -> unit = skip_ml_comment_inner 1

let rec skip_c_comment (lexbuf : S.lexbuf) : unit =
  match%sedlex lexbuf with
  | "*/" ->
      ()
  | '*' ->
      skip_c_comment lexbuf
  | eof ->
      Frontend.lex_error "eof in skip_c_comment" lexbuf
  | Plus (Compl ('*' | eof)) ->
      skip_c_comment lexbuf
  | _ ->
      F.lex_error "failure in skip_c_comment" lexbuf

let rec skip_line (lexbuf : S.lexbuf) : unit =
  match%sedlex lexbuf with
  | eol | eof ->
      ()
  | Plus (Compl (eol | eof)) ->
      skip_line lexbuf
  | _ ->
      F.lex_error "failure in skip_line" lexbuf

let rec skip_string (lexbuf : S.lexbuf) : unit =
  match%sedlex lexbuf with
  | '"' ->
      ()
  | eol ->
      F.lex_error "newline in skip_string" lexbuf
  | eof ->
      F.lex_error "eof in skip_string" lexbuf
  | _ ->
      skip_string lexbuf

type char_result = Single | Double | Char of char | String of string

let read_hex_escape (lexbuf : S.lexbuf) ~(k : char_result -> 'a) : 'a =
  match%sedlex lexbuf with
  | hex_digit, hex_digit ->
      let digits = S.Utf8.lexeme lexbuf in
      (* Int.Hex.of_string won't take raw digits. *)
      let char_int = Int.Hex.of_string ("0x" ^ digits) in
      let char = Char.of_int_exn char_int in
      k (Char char)
  | _ ->
      F.lex_error
        (Printf.sprintf "Invalid hex escape sequence: '%s'"
           (S.Utf8.lexeme lexbuf))
        lexbuf

let read_escape (lexbuf : S.lexbuf) ~(k : char_result -> 'a) : 'a =
  match%sedlex lexbuf with
  | '\'' ->
      k (Char '\'')
  | '"' ->
      k (Char '"')
  (* TODO(@MattWindsor91): octal escapes *)
  | '0' ->
      k (Char '\x00')
  | '\\' ->
      k (Char '\\')
  | 'x' ->
      read_hex_escape lexbuf ~k
  | _ ->
      F.lex_error
        (Printf.sprintf "Invalid escape sequence: '%s'"
           (S.Utf8.lexeme lexbuf))
        lexbuf

let read_string_or_char_inner (lexbuf : S.lexbuf) ~(k : char_result -> 'a) :
    'a =
  match%sedlex lexbuf with
  | '\'' ->
      k Single
  | '"' ->
      k Double
  | '\\' ->
      read_escape ~k lexbuf
  (* TODO(@MattWindsor91): all other escapes *)
  | Plus (Compl ('\'' | '"' | '\\')) ->
      k (String (S.Utf8.lexeme lexbuf))
  | _ ->
      F.lex_error
        (Printf.sprintf "Invalid char/string-literal character: '%s'"
           (S.Utf8.lexeme lexbuf))
        lexbuf

(* per 'Real World OCaml' *)
let rec read_string_inner (tok : string -> 't) (buf : Buffer.t)
    (lexbuf : S.lexbuf) : 't =
  read_string_or_char_inner lexbuf ~k:(function
    | Single ->
        Buffer.add_char buf '\'' ;
        read_string_inner tok buf lexbuf
    | Double ->
        tok (Buffer.contents buf)
    | Char c ->
        Buffer.add_char buf c ;
        read_string_inner tok buf lexbuf
    | String s ->
        Buffer.add_string buf s ;
        read_string_inner tok buf lexbuf)

let read_string (tok : string -> 't) : S.lexbuf -> 't =
  read_string_inner tok (Buffer.create 17)

let read_char_end (tok : string -> 't) (lexbuf : S.lexbuf)
    (contents : string) : 't =
  match%sedlex lexbuf with
  | '\'' ->
      tok contents
  | _ ->
      F.lex_error "Char literal can only contain one character" lexbuf

let read_char (tok : string -> 't) (lexbuf : S.lexbuf) : 't =
  read_string_or_char_inner lexbuf ~k:(function
    | Single ->
        F.lex_error "No character in character literal" lexbuf
    | Double ->
        read_char_end tok lexbuf "\""
    | Char c ->
        read_char_end tok lexbuf (String.of_char c)
    | String s ->
        read_char_end tok lexbuf s)

let escape_string_char (buf : Buffer.t) (lexeme : string) : unit =
  if String.length lexeme = 1 then
    let chr = lexeme.[0] in
    if Char.is_print chr then Buffer.add_char buf chr
    else Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.to_int chr))
  else
    (* TODO(@MattWindsor91): unicode escapes *)
    Buffer.add_string buf lexeme

let rec escape_string_inner (buf : Buffer.t) (lexbuf : S.lexbuf) : string =
  match%sedlex lexbuf with
  | '\\' ->
      Buffer.add_string buf "\\\\" ;
      escape_string_inner buf lexbuf
  | '"' ->
      Buffer.add_string buf "\\\"" ;
      escape_string_inner buf lexbuf
  | '\'' ->
      Buffer.add_string buf "\\'" ;
      escape_string_inner buf lexbuf
  (* TODO(@MattWindsor91): octal escapes *)
  | '\x00' ->
      Buffer.add_string buf "\\0" ;
      escape_string_inner buf lexbuf
  | any ->
      escape_string_char buf (S.Utf8.lexeme lexbuf) ;
      escape_string_inner buf lexbuf
  | eof ->
      Buffer.contents buf
  | _ ->
      F.lex_error "Unexpected char when escaping:" lexbuf

let escape_string (s : string) : string =
  escape_string_inner
    (Buffer.create (String.length s))
    (S.Utf8.from_string s)
