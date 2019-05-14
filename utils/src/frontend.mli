(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Lexing

module Error_range : sig
  type t = position * position [@@deriving sexp_of]
end

exception LexError of string * Error_range.t

val lex_error : string -> lexbuf -> 'a
(** [lex_error error lexbuf] raises a [LexError] with the given error
    message and lexbuf. *)

(** [Basic] is the signature that language frontends must implement to use
    [Make]. *)
module type Basic = sig
  type ast

  module I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  val lex : lexbuf -> I.token

  val parse : position -> ast I.checkpoint

  val message : int -> string
end

(** [S] is the signature of language frontends. *)
module type S = sig
  (** [ast] is the type of the syntax tree outputted by the frontend. *)
  type ast

  include Loadable.S with type t := ast
end

(** [Make] lifts an instance of [B] into a frontend. *)
module Make (B : Basic) : S with type ast := B.ast
