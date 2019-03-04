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

(** Bundle of formatters used to output information in the middle of
    act execution *)

open Core

(** [t] is the type of output contexts. *)
type t =
  { vf : Format.formatter (* Verbose formatter *)
  ; wf : Format.formatter (* Warning formatter *)
  ; ef : Format.formatter (* Error formatter *)
  }

(** [make] makes a [t] from various reporting flags. *)
val make : verbose:bool -> warnings:bool -> t

(** [log_stage o ~stage ~file compiler_id] outputs a brief
    notice, onto [o]'s verbose formatter, that explains that the
    stage named [stage] is happening on file [file] and compiler ID
    [compiler_id]. *)
val log_stage
  :  t
  -> stage:string
  -> file:string
  -> Id.t
  -> unit
;;

(** [print_error o u] prints any top-level errors represented by [u]
   to [o]'s error formatter. *)
val print_error : t -> unit Or_error.t -> unit
