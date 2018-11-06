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

(** [Analysis] contains abstract data types for storing finished
   analyses of programs. *)

open Core
open Utils

(** [Timed] is a signature for abstract data types containing a
    time-taken span. *)
module type Timed = sig
  type t

  (** [time_taken t] gets the time taken to produce analysis [t]. *)
  val time_taken : t -> Time.Span.t
end

module Herd : sig
  (** [t] is the type of Herd analysis runs. *)
  type t =
    [ Herd_output.outcome
    | `Disabled
    | `Errored of [`C | `Assembly]
    ] [@@deriving sexp_of]
  ;;
end

module File : sig
  (** [t] is the opaque type of a single-file analysis. *)
  type t [@@deriving sexp_of]

  include Timed with type t := t

  (** [herd f] gets the Herd result for file [f]. *)
  val herd : t -> Herd.t

  (** [create ~herd ~time_taken] creates a file analysis
      given Herd analysis [herd] and time taken [time_taken]. *)
  val create
    :  herd       : Herd.t
    -> time_taken : Time.Span.t
    -> t
end

module Compiler : sig
  (** [t] is the opaque type of a single-compiler analysis. *)
  type t [@@deriving sexp_of]

  include Timed with type t := t

  (** [files c] gets an associative list of all analysis results
      for files on compiler [c]. *)
  val files : t -> (string, File.t) List.Assoc.t

  (** [create ~files ~time_taken] creates a compiler analysis given
     file analyses [files] and total time taken in compiler
     [time_taken]. *)
  val create
    :  files      : (string, File.t) List.Assoc.t
    -> time_taken : Time.Span.t
    -> t
end

module Machine : sig
  (** [t] is the opaque type of a machine analysis. *)
  type t [@@deriving sexp_of]

  include Timed with type t := t

  (** [compilers m] gets an associative list of all analysis results
      for compilers on machine [m]. *)
  val compilers : t -> (Spec.Id.t, Compiler.t) List.Assoc.t

  (** [files m] gets a list of (compiler ID, filename, analysis)
     tuples for all files processed on machine [m]. *)
  val files : t -> (Spec.Id.t * string * File.t) list

  (** [create ~compilers ~time_taken] creates a compiler analysis
     given compiler analyses [compilers] and total time taken on
     machine [time_taken]. *)
  val create
    :  compilers  : (Spec.Id.t, Compiler.t) List.Assoc.t
    -> time_taken : Time.Span.t
    -> t
  ;;
end

(** [t] is the opaque type of a full analysis. *)
type t [@@deriving sexp_of]

(** [create ~machines ~time_taken] creates a top-level analysis given
   machine analyses [machines] and total time taken [time_taken]. *)
val create
  :  machines  : (Spec.Id.t, Machine.t) List.Assoc.t
  -> time_taken : Time.Span.t
  -> t
;;

(** [files x] gets a list of (machine ID, compiler ID, filename,
   analysis) tuples for all files in analysis [x]. *)
val files : t -> (Spec.Id.t * Spec.Id.t * string * File.t) list

include Tabulator.Tabular with type data := t

(** [machines t] gets an associative list of all analysis results for
    machines in run [t]. *)
val machines : t -> (Spec.Id.t, Machine.t) List.Assoc.t
