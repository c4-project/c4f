(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** High-level front-end for assembly translation jobs

    [Job] specifies a signature, [Runner], that describes a module that
    takes an act job specification (of type [t]) and, on success,
    produces output (of type [output]).  Such [Runner]s abstract over
    all of the I/O plumbing and other infrastructure needed to do
    the jobs. *)

open Core

include module type of Asm_job_intf

(** [t] is a description of a single-file job. *)
type 'fmt t

val make
  :  ?format:'fmt
  -> ?passes:Sanitiser_pass.Set.t
  -> ?symbols:string list
  -> unit
  -> 'fmt t
(** [make ?format ?passes ?symbols ()] makes a job description. *)


module Litmus_format : sig
  (** [t] is an enumeration of output formats for litmus jobs. *)
  type t =
    | Full           (** Output a full, herd-compatible litmus test *)
    | Programs_only  (** Only output the program tables (eg for comparison) *)
  [@@deriving eq]
  ;;

  (** [default] gets the default output format. *)
  val default : t
end

module Explain_format : sig
  (** [explain_format] is an enumeration of output formats for explain jobs. *)
  type t =
    | Assembly  (** Terse, but as close to parseable assembly as possible *)
    | Detailed  (** More details than [Assembly], but verbose and free-form *)
  [@@deriving eq]
  ;;

  (** [default] gets the default output format. *)
  val default : t
end

(** [output] is the output of a single-file job. *)
type output

(** [symbol_map o] returns the mapping from pre-compiler
    symbols (given in the input [t]) to mangled
    assembly symbols for the job output [o]. *)
val symbol_map : output -> (string, string) List.Assoc.t

(** [warn o f] prints any warnings attached to output [o] on
    pretty-print formatter [f]. *)
val warn : output -> Format.formatter -> unit

module type Runner =
  Gen_runner with type 'fmt inp := 'fmt t
              and type aux := output
              and type lfmt := Litmus_format.t
              and type efmt := Explain_format.t
(** Signature of job runners. *)

module Make_runner (R : Runner_deps) : Runner
(** [Make_runner] makes a [Runner] from a [Runner_deps] module. *)

(** {2 First-class wrappers for getting job runners } *)

val get_litmusify
  :  (module Runner)
  -> (module Utils.Filter.S with type aux_i = Litmus_format.t t
                             and type aux_o = output
     )
(** [get_litmusify Runner] is [Runner.Litmusify]. *)

val get_explain
  :  (module Runner)
  -> (module Utils.Filter.S with type aux_i = Explain_format.t t
                             and type aux_o = output
     )
(** [get_explain Runner] is [Runner.Explain]. *)
