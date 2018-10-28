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
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Utils
open Lexing

let pp_pos f (pos : position) =
  Format.fprintf f "@[%s:%d:%d]"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
;;

let pp_pos2 f ((pos1, pos2) : position * position) =
  Format.fprintf f "@[%s:%d:%d-%d:%d]"
    pos1.pos_fname
    pos1.pos_lnum
    (pos1.pos_cnum - pos1.pos_bol)
    pos2.pos_lnum
    (pos2.pos_cnum - pos2.pos_bol)
;;

let sprint_pos : position -> string = Format.asprintf "%a" pp_pos;;

exception LexError of string * position

let lex_error msg lexbuf = raise (LexError (msg, lexbuf.lex_curr_p));;

module type S = sig
  type ast

  module I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  val lex : lexbuf -> I.token;;
  val parse : position -> ast I.checkpoint;;
  val message : int -> string;;
end

module type Intf = sig
  type ast

  include Io.LoadableIntf with type t := ast;;
end

module Make (SI : S) : Intf with type ast = SI.ast = struct
  type ast = SI.ast

  let fail (lexbuf : Lexing.lexbuf) = function
    | SI.I.HandlingError env ->
      let state = SI.I.current_state_number env in
      let msg = SI.message state in
      Or_error.errorf "%a: %s" (Fn.const sprint_pos) lexbuf.lex_curr_p msg
    | _ -> assert false
  ;;

  let loop lexbuf checkpoint =
    let supplier = SI.I.lexer_lexbuf_to_supplier SI.lex lexbuf in
    SI.I.loop_handle Or_error.return (fail lexbuf) supplier checkpoint
  ;;

  let parse lexbuf =
    try
      loop lexbuf (SI.parse lexbuf.lex_curr_p)
    with
    | LexError (error, position) ->
      Or_error.error_s [%message "Lexing error" ~position:(sprint_pos position) ~error]
  ;;

  module Load : (Io.LoadableS with type t = ast) = struct
    type t = ast;;

    let load_from_ic ?(path="(stdin)") ic =
      let lexbuf = from_channel ic in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path };
      parse lexbuf
    ;;

    let load_from_string str =
      let lexbuf = from_string str in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(string)" };
      parse lexbuf
    ;;
  end

  include Io.LoadableMake(Load)
end
