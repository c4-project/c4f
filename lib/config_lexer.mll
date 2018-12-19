(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

{
open Core_kernel
open Config_parser
open Lexing

open Frontend
open Lex_utils

let keywords = lazy
  (
    String.Caseless.Map.of_alist_exn
      [ "argv", ARGV
      ; "asm_model", ASM_MODEL
      ; "c_model", C_MODEL
      ; "cmd", CMD
      ; "compiler", COMPILER
      ; "copy", COPY
      ; "default", DEFAULT
      ; "emits", EMITS
      ; "enabled", ENABLED
      ; "false", BOOL false
      ; "herd", HERD
      ; "host", HOST
      ; "local", LOCAL
      ; "machine", MACHINE
      ; "no", BOOL false
      ; "on", BOOL true
      ; "off", BOOL false
      ; "ssh", SSH
      ; "style", STYLE
      ; "to", TO
      ; "true", BOOL true
      ; "user", USER
      ; "via", VIA
      ; "yes", BOOL true
      ]
  )
}

let digit = [ '0'-'9' ]
let hex   = [ '0'-'9' 'a'-'f' 'A'-'F' ]
let alpha = [ 'a'-'z' 'A'-'Z' ]

let id_start   = (alpha|'_')
let id_middle  = (id_start|digit|'.')
let id         = id_start id_middle*

rule token = parse
  | [ ' ''\t''\r' ] { token lexbuf }
  | '\n' { new_line lexbuf; EOL }
  | '#' { skip_c_line_comment lexbuf; EOL }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '"' { read_string (fun x -> STRING x) lexbuf }
  | id as x {
      match String.Caseless.Map.find (force keywords) x with
      | Some token -> token
      | None -> IDENTIFIER (Id.of_string x)
    }
  | eof { EOF }
  | _ { lex_error ("Unexpected char: " ^ Lexing.lexeme lexbuf) lexbuf }
