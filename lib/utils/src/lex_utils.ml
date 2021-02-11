(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* This file derives in part from the Herd7 project
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

let eol = [%sedlex.regexp? '\n']

let rec skip_c_comment (lexbuf : Sedlexing.lexbuf) : unit =
  match%sedlex lexbuf with
  | "*/" -> ()
  | '*' -> skip_c_comment lexbuf
  | eof -> Frontend.lex_error "eof in skip_c_comment" lexbuf
  | Plus (Compl ('*' | eof)) -> skip_c_comment lexbuf
  | _ -> Frontend.lex_error "failure in skip_c_comment" lexbuf

let rec skip_line (lexbuf : Sedlexing.lexbuf) : unit =
  match%sedlex lexbuf with
  | eol | eof -> ()
  | Plus (Compl (eol | eof)) -> skip_line lexbuf
  | _ -> Frontend.lex_error "failure in skip_line" lexbuf

let rec skip_string (lexbuf : Sedlexing.lexbuf) : unit =
  match%sedlex lexbuf with
  | '"' -> ()
  | eol -> Frontend.lex_error "newline in skip_string" lexbuf
  | eof -> Frontend.lex_error "eof in skip_string" lexbuf
  | _ -> skip_string lexbuf

type char_result = Single | Double | Char of char | String of string

let read_hex_escape (lexbuf : Sedlexing.lexbuf) ~(k : char_result -> 'a) : 'a
    =
  match%sedlex lexbuf with
  | hex_digit, hex_digit ->
      let digits = Sedlexing.Utf8.lexeme lexbuf in
      (* Int.Hex.of_string won't take raw digits. *)
      let char_int = Int.Hex.of_string ("0x" ^ digits) in
      let char = Char.of_int_exn char_int in
      k (Char char)
  | _ ->
      Frontend.lex_error
        (Printf.sprintf "Invalid hex escape sequence: '%s'"
           (Sedlexing.Utf8.lexeme lexbuf) )
        lexbuf

let read_escape (lexbuf : Sedlexing.lexbuf) ~(k : char_result -> 'a) : 'a =
  match%sedlex lexbuf with
  | '\'' -> k (Char '\'')
  | '"' -> k (Char '"')
  (* TODO(@MattWindsor91): octal escapes *)
  | '0' -> k (Char '\x00')
  | '\\' -> k (Char '\\')
  | 'x' -> read_hex_escape lexbuf ~k
  | _ ->
      Frontend.lex_error
        (Printf.sprintf "Invalid escape sequence: '%s'"
           (Sedlexing.Utf8.lexeme lexbuf) )
        lexbuf

let read_string_or_char_inner (lexbuf : Sedlexing.lexbuf)
    ~(k : char_result -> 'a) : 'a =
  match%sedlex lexbuf with
  | '\'' -> k Single
  | '"' -> k Double
  | '\\' -> read_escape ~k lexbuf
  (* TODO(@MattWindsor91): all other escapes *)
  | Plus (Compl ('\'' | '"' | '\\')) ->
      k (String (Sedlexing.Utf8.lexeme lexbuf))
  | _ ->
      Frontend.lex_error
        (Printf.sprintf "Invalid char/string-literal character: '%s'"
           (Sedlexing.Utf8.lexeme lexbuf) )
        lexbuf

(* per 'Real World OCaml' *)
let rec read_string_inner (tok : string -> 't) (buf : Buffer.t)
    (lexbuf : Sedlexing.lexbuf) : 't =
  read_string_or_char_inner lexbuf ~k:(function
    | Single ->
        Buffer.add_char buf '\'' ;
        read_string_inner tok buf lexbuf
    | Double -> tok (Buffer.contents buf)
    | Char c ->
        Buffer.add_char buf c ;
        read_string_inner tok buf lexbuf
    | String s ->
        Buffer.add_string buf s ;
        read_string_inner tok buf lexbuf )

let read_string (tok : string -> 't) : Sedlexing.lexbuf -> 't =
  read_string_inner tok (Buffer.create 17)

let read_char_end (tok : string -> 't) (lexbuf : Sedlexing.lexbuf)
    (contents : string) : 't =
  match%sedlex lexbuf with
  | '\'' -> tok contents
  | _ ->
      Frontend.lex_error "Char literal can only contain one character" lexbuf

let read_char (tok : string -> 't) (lexbuf : Sedlexing.lexbuf) : 't =
  read_string_or_char_inner lexbuf ~k:(function
    | Single -> Frontend.lex_error "No character in character literal" lexbuf
    | Double -> read_char_end tok lexbuf "\""
    | Char c -> read_char_end tok lexbuf (String.of_char c)
    | String s -> read_char_end tok lexbuf s )

let escape_string_char (buf : Buffer.t) (lexeme : string) : unit =
  if String.length lexeme = 1 then
    let chr = lexeme.[0] in
    if Char.is_print chr then Buffer.add_char buf chr
    else Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.to_int chr))
  else
    (* TODO(@MattWindsor91): unicode escapes *)
    Buffer.add_string buf lexeme

let escape_string (s : string) : string =
  let rec mu (buf : Buffer.t) (lexbuf : Sedlexing.lexbuf) : string =
    match%sedlex lexbuf with
    | '\\' ->
        Buffer.add_string buf "\\\\" ;
        mu buf lexbuf
    | '"' ->
        Buffer.add_string buf "\\\"" ;
        mu buf lexbuf
    | '\'' ->
        Buffer.add_string buf "\\'" ;
        mu buf lexbuf
    (* TODO(@MattWindsor91): octal escapes *)
    | '\x00' ->
        Buffer.add_string buf "\\0" ;
        mu buf lexbuf
    | any ->
        escape_string_char buf (Sedlexing.Utf8.lexeme lexbuf) ;
        mu buf lexbuf
    | eof -> Buffer.contents buf
    | _ -> Frontend.lex_error "Unexpected char when escaping:" lexbuf
  in
  mu (Buffer.create (String.length s)) (Sedlexing.Utf8.from_string s)
