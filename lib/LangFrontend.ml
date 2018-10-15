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

module type S = sig
  type ast
  type token

  val lex : Lexing.lexbuf -> token
  val parse
    :  (Lexing.lexbuf -> token)
    -> Lexing.lexbuf
    -> ast Or_error.t
end

module type Intf = sig
  type ast

  val run_ic : ?file:string -> In_channel.t -> ast Or_error.t

  val run_file : file:string -> ast Or_error.t
  val run_stdin : unit -> ast Or_error.t
end

module Make (SI : S) =
  struct
    type ast = SI.ast

    let run_ic ?file ic =
      let lexbuf = Lexing.from_channel ic in
      Option.iter
        ~f:(fun f ->
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f })
        file;
      SI.parse SI.lex lexbuf

    let run_file ~file =
      In_channel.with_file file ~f:(run_ic ~file)

    let run_stdin () =
      run_ic ~file:"(stdin)" In_channel.stdin
  end
