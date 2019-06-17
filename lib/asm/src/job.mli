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

(** High-level front-end for assembly translation jobs

    [Job] specifies a signature, [Runner], that describes a module that
    takes an act job specification (of type [t]) and, on success, produces
    output (of type [output]). Such [Runner]s abstract over all of the I/O
    plumbing and other infrastructure needed to do the jobs. *)

open Base

(** [t] is a description of a single-file job. *)
type 'cfg t

val make :
     ?config:'cfg
  -> ?passes:Set.M(Act_sanitiser.Pass_group).t
  -> ?aux:Act_delitmus.Aux.t
  -> unit
  -> 'cfg t
(** [make ?config ?passes ?symbols ()] makes a job description. *)

val map_m_config : 'a t -> f:('a -> 'b Or_error.t) -> 'b t Or_error.t

val config : 'cfg t -> 'cfg option

val passes : _ t -> Set.M(Act_sanitiser.Pass_group).t

val symbols : _ t -> string list

(** The output of a single-file job. *)
module Output : sig
  type t

  val symbol_map : t -> (string, string) List.Assoc.t
  (** [symbol_map o] returns the mapping from pre-compiler symbols (given in
      the input [t]) to mangled assembly symbols for the job output [o]. *)

  val warn : t Fmt.t
  (** [warn f o] prints any warnings attached to output [o] on pretty-print
      formatter [f]. *)

  val make :
       'warn Fmt.t
    -> string
    -> (string, string) List.Assoc.t
    -> 'warn list
    -> t
  (** [make pp_warn iname symbol_map warns] makes a job output. *)
end
