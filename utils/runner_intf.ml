(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base
open Stdio

(** Signature that runner modules must implement to use
   {{!Run.Make}Make}. *)
module type Basic = sig
  val run_batch
    :  ?oc:Out_channel.t
    -> prog:string
    -> string list list
    -> unit Or_error.t
    (** [run_batch ?oc ~prog argss] runs [prog] on each argument
       vector in [argss], waiting for each invocation to complete in
       sequence, and collecting any errors as [Error.t]s.

        If [oc] is given, each process's standard output will be
       copied line-by-line to it as it ends. *)
end

(** Outward-facing interface of process runners. *)
module type S = sig
  include Basic

  val run
    :  ?oc:Out_channel.t
    -> prog:string
    -> string list
    -> unit Or_error.t
  (** [run ?oc ~prog ~args] runs the given program, waits for it to
      complete, and translates any errors to [Error.t].

      If [oc] is given, the process's standard output will be copied
      line-by-line to it at the end. *)
end
