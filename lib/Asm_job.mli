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
open Utils

(** [t] is a description of a single-file job. *)
type t =
  { inp     : Io.In_source.t
  ; outp    : Io.Out_sink.t
  ; passes  : Sanitiser_pass.Set.t
  ; symbols : string list
  }
;;

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

(** [Runner] is the signature of job runners. *)
module type Runner = sig
  (** [litmusify ?output_format t] runs a litmusify job using [t].
      If [output_format] is given, it overrides the default
      ([Litmus_format.default]). *)
  val litmusify
    :  ?output_format:Litmus_format.t
    -> t
    -> output Or_error.t
  ;;

  (** [explain ?output_format t] runs an explain job over [t].
      If [output_format] is given, it overrides the default
      ([Output.default]). *)
  val explain
    :  ?output_format:Explain_format.t
    -> t
    -> output Or_error.t
  ;;
end

(** [Runner_deps] is a signature bringing together the modules we
    need to be able to run single-file jobs. *)
module type Runner_deps = sig
  type ast

  (** [Lang] is the main language used in the jobs, which may differ
      from the [Litmus] language. *)
  module Lang : Language.S

  module Frontend : Frontend.S with type ast := ast
  module Litmus : Litmus.S
  module Multi_sanitiser
    : Sanitiser.S with module Lang := Lang
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with module Lang := Lang
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer : Explainer.S with module Lang := Lang

  val final_convert : Lang.Statement.t list -> Litmus.Lang.Statement.t list

  val statements : ast -> Lang.Statement.t list
end

(** [Make_runner] makes a [Runner] from a [Runner_deps] module. *)
module Make_runner : functor (R : Runner_deps) -> Runner
