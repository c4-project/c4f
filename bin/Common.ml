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
open Lib
open Utils

let do_litmusify mode passes o ~infile ~outfile spec =
  let open Result.Let_syntax in
  let%bind lit = LangSupport.get_litmusifier ~emits:spec.CompilerSpec.emits in
  Io.(
    let f src inp _ outp =
      let iname = MyFormat.format_to_string (In_source.pp) src in
      let module L = (val lit) in
      L.run
        { o
        ; iname
        ; inp
        ; outp
        ; mode
        ; passes
        }
    in
    with_input_and_output
      (In_source.of_option infile)
      (Out_sink.of_option outfile)
      ~f
  )
;;

let print_error =
  Result.iter_error
    ~f:(Format.eprintf "@[act encountered a top-level error:@.@[%a@]@]@." Error.pp)
;;
