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

open Core
open Lexing

let pp_pos f (pos : position) =
  Format.fprintf f "@[%s:%d:%d]"
                 pos.pos_fname
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol)

let pp_pos2 f ((pos1, pos2) : position * position) =
  Format.fprintf f "@[%s:%d:%d-%d:%d]"
                 pos1.pos_fname
                 pos1.pos_lnum
                 (pos1.pos_cnum - pos1.pos_bol)
                 pos2.pos_lnum
                 (pos2.pos_cnum - pos2.pos_bol)

exception Error of string * position

let error msg lexbuf = raise (Error (msg,lexbuf.lex_curr_p ))
