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
open Utils

(** [compile_with_compiler c o ~name ~infile ~outfile compiler_id]
    compiles [infile] (with short name [name]) to [outfile], using
    compiler module [c].  In addition, it does some book-keeping
    and logging, including stage-logging with [compiler_id], and
    recording and returning the duration spent in the compiler. *)
val compile_with_compiler
  :  (module Compiler.S)
  -> Output.t
  -> name:string
  -> infile:string
  -> outfile:string
  -> Spec.Id.t
  -> Time.Span.t Or_error.t
;;

(** [lift_command ?local_only ?test_compilers ~f standard_args] lifts
   a command body [f], performing common book-keeping such as loading
   and testing the configruation, creating an [Output.t], and printing
   top-level errors. *)
val lift_command
  :  ?local_only:bool (* defaults to false *)
  -> ?test_compilers:bool (* defaults to false *)
  -> f:(Output.t -> Config.M.t -> unit Or_error.t)
  -> Standard_args.t
  -> unit
;;

(** [litmusify ?programs_only o inp outp symbols spec] is a thin
   wrapper around [Asm_job]'s litmusify mode that handles finding the
   right job runner, printing warnings, and supplying the maximal pass
   set. *)
val litmusify
  :  ?programs_only:bool
  -> Output.t
  -> Io.In_source.t
  -> Io.Out_sink.t
  -> string list
  -> Compiler.Full_spec.With_id.t
  -> (string, string) List.Assoc.t Or_error.t
;;
