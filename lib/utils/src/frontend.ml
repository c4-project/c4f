(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Sedlexing

module Error_range = struct
  open Lexing

  type t = position * position

  (* range 'ends in a newline character', and also contains ':' *)
  let to_string = MenhirLib.LexerUtil.range
end

exception LexError of string * Error_range.t

let lex_error msg lexbuf =
  raise (LexError (msg, Sedlexing.lexing_positions lexbuf))

module type Basic = sig
  type ast

  module I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  val lex : lexbuf -> I.token

  val parse : Lexing.position -> ast I.checkpoint

  val message : int -> string
end

module Make (B : Basic) : Plumbing.Loadable_types.S with type t = B.ast =
struct
  let env : B.ast B.I.checkpoint -> B.ast B.I.env = function
    | B.I.HandlingError env -> env
    | _ -> assert false

  let err (pos : Error_range.t) (ty : string) (details : string) :
      'a Or_error.t =
    Or_error.errorf "%s%s error: %s" (Error_range.to_string pos) ty details

  let fail (_lexbuf : lexbuf) (cp : B.ast B.I.checkpoint) : B.ast Or_error.t
      =
    let env = env cp in
    let state = B.I.current_state_number env in
    let position : Error_range.t = B.I.positions env in
    (* Can't easily use the error reporting stuff here, as we often load from
       stdin? *)
    let details = B.message state in
    err position "parse" details

  let loop (lexbuf : lexbuf) (checkpoint : B.ast B.I.checkpoint) :
      B.ast Or_error.t =
    let supplier = Sedlexing.with_tokenizer B.lex lexbuf in
    B.I.loop_handle Or_error.return (fail lexbuf) supplier checkpoint

  let parse (lexbuf : lexbuf) =
    let _, cur_pos = Sedlexing.lexing_positions lexbuf in
    try loop lexbuf (B.parse cur_pos)
    with LexError (details, position) -> err position "lexing" details

  module Load : Plumbing.Loadable_types.Basic with type t = B.ast = struct
    type t = B.ast

    let load_from_ic ?(path = "(stdin)") ic =
      let lexbuf = Utf8.from_channel ic in
      set_filename lexbuf path ; parse lexbuf

    let load_from_string str =
      let lexbuf = Utf8.from_string str in
      set_filename lexbuf "(string)" ;
      parse lexbuf
  end

  include Plumbing.Loadable.Make (Load)
end
