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

(** Glue code common to all top-level commands *)

open Core
open Lib

(** [do_litmusify] finds the right [Litmusifier] for a compiler spec,
    and runs it with the given arguments.  On success, it returns the
    mapping from symbols in [symbols] to their counterparts in the
    litmus test. *)
val do_litmusify
  :  [`Litmusify | `Explain]
  -> Sanitiser_pass.Set.t
  -> OutputCtx.t
  -> ?symbols : string list
  -> infile : string option
  -> outfile : string option
  -> Compiler.CSpec.t
  -> (string, string) List.Assoc.t Or_error.t
;;

(** [print_error u] prints any top-level errors represented by [u] to
   stderr. *)
val print_error : unit Or_error.t -> unit;;
