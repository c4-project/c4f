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

open Lexing
open Utils

type error_range = (position * position) [@@deriving sexp_of];;

exception LexError of string * error_range;;

(** [lex_error error lexbuf] raises a [LexError] with the given
    error message and lexbuf. *)
val lex_error : string -> lexbuf -> 'a;;

module type S = sig
  type ast

  module I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  val lex : lexbuf -> I.token;;
  val parse : position -> ast I.checkpoint;;
  val message : int -> string;;
end

module type Intf = sig
  (** [ast] is the type of the syntax tree outputted by the frontend. *)
  type ast

  include Io.LoadableIntf with type t := ast;;
end

(** [Make] lifts an instance of [S] into a frontend. *)
module Make : functor (SI : S) -> Intf with type ast = SI.ast
