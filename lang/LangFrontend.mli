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

module type S =
  sig
    (** [ast] is the type of the syntax tree outputted by the frontend. *)
    type ast
    (** [perr] is the type of parse errors. *)
    type perr
    (** [lerr] is the type of lex errors. *)
    type lerr

    (** [pp_perr f perr] pretty-prints a parse error [perr] onto formatter [f]. *)
    val pp_perr : Format.formatter -> perr -> unit

    (** [pp_perr f perr] pretty-prints a lex error [lerr] onto formatter [f]. *)
    val pp_lerr : Format.formatter -> lerr -> unit

    (** [run_ic file ic] runs the language frontend on input channel
     [ic].  [file] can be set to pass in a filename, for error
     reporting purposes. *)
    val run_ic : ?file:string -> In_channel.t -> (ast, (perr, lerr) LangParser.error) result

    (** [run_file file] runs the language frontend on filename [file]. *)
    val run_file : file:string -> (ast, (perr, lerr) LangParser.error) result

    (** [run_stdin ()] runs the language frontend on standard input. *)
    val run_stdin : unit -> (ast, (perr, lerr) LangParser.error) result
  end

(** [Make] lifts an instance of [LangParser.S] into a frontend. *)
module Make : functor (SI : LangParser.S) -> S with type ast = SI.ast
