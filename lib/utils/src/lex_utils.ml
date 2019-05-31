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

(* per 'Real World OCaml' *)
let rec read_string_inner (tok : string -> 't) (buf : Buffer.t)
    (lexbuf : S.lexbuf) : 't =
  match%sedlex lexbuf with
  | '"' ->
      tok (Buffer.contents buf)
  | '\\', '\\' ->
      Buffer.add_char buf '\\' ;
      read_string_inner tok buf lexbuf
  | '\\', '0' ->
      Buffer.add_char buf '\x00' ;
      read_string_inner tok buf lexbuf
  | Plus (Compl ('"' | '\\')) ->
      Buffer.add_string buf (S.Utf8.lexeme lexbuf) ;
      read_string_inner tok buf lexbuf
  | _ ->
      F.lex_error
        ("Invalid string character: " ^ S.Utf8.lexeme lexbuf)
        lexbuf

let read_string (tok : string -> 't) : S.lexbuf -> 't =
  read_string_inner tok (Buffer.create 17)
