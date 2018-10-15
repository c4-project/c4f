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
 open LexMisc

module type Config = sig
  val debug : bool
end

module Default = struct
  let debug = false
end

module Make(O:Config) = struct
}

let digit = ['0'-'9']
let num = digit+
let blank = ['\t'' ']
rule skip_comment i = parse
  | '\n' { new_line lexbuf; skip_comment i lexbuf }
  | "(*" { skip_comment (i+1) lexbuf }
  | "*)"
      { if i > 1 then skip_comment (i-1) lexbuf}
  | eof { error "eof in skip_comment" lexbuf }
  | _ { skip_comment i lexbuf}

and skip_c_comment = parse
  | '\n' { new_line lexbuf; skip_c_comment lexbuf }
  | "*/" { () }
  | eof { error "eof in skip_c_comment" lexbuf }
  | _ { skip_c_comment lexbuf}

and skip_c_line_comment = parse
  | '\n' { new_line lexbuf }
  | eof { () }
  | _ { skip_c_line_comment lexbuf}

and skip_string = parse
  | '\n' 	{ error "newline in skip_string" lexbuf }
  | "\""	{ () }
  | eof 	{ error "eof in skip_string" lexbuf }
  | _ 		{ skip_string lexbuf}

{

let skip_comment = skip_comment 1
end
}
