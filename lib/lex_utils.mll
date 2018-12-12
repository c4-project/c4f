(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor (parts (c) 2010-2018 Institut
   National de Recherche en Informatique et en Automatique, Jade
   Alglave, and Luc Maranget)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   This file derives from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
open Lexing
open Frontend
}

rule skip_ml_comment i = parse
  | '\n' { new_line lexbuf; skip_ml_comment i lexbuf }
  | "(*" { skip_ml_comment (i+1) lexbuf }
  | "*)"
      { if i > 1 then skip_ml_comment (i-1) lexbuf}
  | eof { lex_error "eof in skip_ml_comment" lexbuf }
  | _ { skip_ml_comment i lexbuf}

and skip_c_comment = parse
  | '\n' { new_line lexbuf; skip_c_comment lexbuf }
  | "*/" { () }
  | eof { lex_error "eof in skip_c_comment" lexbuf }
  | _ { skip_c_comment lexbuf}

and skip_c_line_comment = parse
  | '\n' { new_line lexbuf }
  | eof { () }
  | _ { skip_c_line_comment lexbuf}

and skip_string = parse
  | '\n' 	{ lex_error "newline in skip_string" lexbuf }
  | "\""	{ () }
  | eof 	{ lex_error "eof in skip_string" lexbuf }
  | _ 		{ skip_string lexbuf}

{
let skip_ml_comment = skip_ml_comment 1
}
