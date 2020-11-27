(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Sedlexing

module Error_range = struct
  module M = struct
    open Lexing

    type t = position * position

    let from_file (from, _) = from.pos_fname

    let from_line (from, _) = from.pos_lnum

    let until_line (_, until) = until.pos_lnum

    let column lpos = lpos.pos_cnum - lpos.pos_bol + 1

    let from_column (from, _) = column from

    let until_column (_, until) = column until

    let file_from_until pos =
      (* assuming both positions refer to the same file *)
      ( from_file pos
      , ((from_line pos, from_column pos), (until_line pos, until_column pos))
      )

    let colon = Fmt.(const char ':')

    let pp = Fmt.(using file_from_until (pair ~sep:colon string text_loc))

    let to_string = Fmt.to_to_string pp

    let of_string _ = failwith "unimplemented"
  end

  include M
  include Sexpable.Of_stringable (M)
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
  let fail (_lexbuf : lexbuf) = function
    | B.I.HandlingError env ->
        let state = B.I.current_state_number env in
        let details = B.message state in
        Or_error.error_s
          [%message
            "Parse error"
              ~position:(B.I.positions env : Error_range.t)
              ~details]
    | _ ->
        assert false

  let loop (lexbuf : lexbuf) checkpoint =
    let supplier = Sedlexing.with_tokenizer B.lex lexbuf in
    B.I.loop_handle Or_error.return (fail lexbuf) supplier checkpoint

  let parse (lexbuf : lexbuf) =
    let _, cur_pos = Sedlexing.lexing_positions lexbuf in
    try loop lexbuf (B.parse cur_pos)
    with LexError (details, position) ->
      Or_error.error_s
        [%message
          "Lexing error" ~position:(position : Error_range.t) ~details]

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
