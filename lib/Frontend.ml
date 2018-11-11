(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
   SOFTWARE. *)

open Core
open Utils
open Lexing

type error_range = (Lexing.position * Lexing.position);;

let sexp_of_error_range ((from, until) : error_range) : Sexp.t =
  Sexp.of_string
    (sprintf "%s:%d:%d-%d:%d"
       from.pos_fname
       from.pos_lnum
       (from.pos_cnum - from.pos_bol)
       until.pos_lnum
       (until.pos_cnum - until.pos_bol))
;;

exception LexError of string * error_range

let lex_error msg lexbuf =
  raise (LexError (msg, (lexbuf.lex_start_p, lexbuf.lex_curr_p)))
;;

module type Basic = sig
  type ast

  module I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  val lex : lexbuf -> I.token;;
  val parse : position -> ast I.checkpoint;;
  val message : int -> string;;
end

module type S = sig
  type ast

  include Loadable.S with type t := ast
end

module Make (B : Basic) : S with type ast := B.ast = struct
  let fail (_lexbuf : Lexing.lexbuf) = function
    | B.I.HandlingError env ->
      let state = B.I.current_state_number env in
      let details = B.message state in
      Or_error.error_s
        [%message "Parse error"
          ~position:(B.I.positions env : error_range)
          ~details
        ]
    | _ -> assert false
  ;;

  let loop lexbuf checkpoint =
    let supplier = B.I.lexer_lexbuf_to_supplier B.lex lexbuf in
    B.I.loop_handle Or_error.return (fail lexbuf) supplier checkpoint
  ;;

  let parse lexbuf =
    try
      loop lexbuf (B.parse lexbuf.lex_curr_p)
    with
    | LexError (details, position) ->
      Or_error.error_s
        [%message "Lexing error"
            ~position:(position : error_range)
            ~details
        ]
  ;;

  module Load : (Loadable.Basic with type t = B.ast) = struct
    type t = B.ast

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

  include Loadable.Make (Load)
end
