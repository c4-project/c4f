(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** [Analysis] contains abstract data types for storing finished analyses of
    programs. *)

open Core_kernel
open Utils
open Lib

(** Represents an observation that the C and assembly sets of a
    state-set-level analysis aren't equal. *)
module State_deviation : sig
  type t [@@deriving sexp_of]

  val in_c_only : t -> Sim_output.State.t list
  (** [in_c_only dev] gets a list (in no particular order) of states that
      were in the C simulation, but not in the assembly one. *)

  val in_asm_only : t -> Sim_output.State.t list
  (** [in_asm_only dev] gets a list (in no particular order) of states that
      were in the C simulation, but not in the assembly one. *)

  val of_herd_outcome_opt : Sim_diff.t -> t option
  (** [of_herd_outcome_opt oc] converts [oc] into a deviation record. It
      returns [None] if the outcome doesn't represent one. *)
end

module Herd : sig
  (** [t] is the type of Herd analysis runs. *)
  type t =
    | Run of Sim_diff.t
        (** Herd ran on both sides, with the given differential result. *)
    | Disabled  (** Herd was disabled. *)
    | Errored of [`C | `Assembly]  (** Herd encountered an error. *)
  [@@deriving sexp_of]

  val to_state_deviation_opt : t -> State_deviation.t option
  (** [to_state_deviation_opt oc] converts [oc] into a deviation record. It
      returns [None] if the outcome doesn't represent one. *)
end

module File : sig
  (** [t] is the opaque type of a single-file analysis. *)
  type t [@@deriving sexp_of]

  include Timing.Timed0 with type t := t

  val herd : t -> Herd.t
  (** [herd f] gets the Herd result for file [f]. *)

  val make :
       ?time_taken:Time.Span.t
    -> ?time_taken_in_cc:Time.Span.t
    -> herd:Herd.t
    -> unit
    -> t
  (** [make ?time_taken ?time_taken_in_cc ()] creates a file analysis given
      Herd analysis [herd], total time taken [time_taken], and time taken in
      the C compiler [time_taken_in_cc]. *)
end

module Compiler : sig
  (** [t] is the opaque type of a single-compiler analysis. *)
  type t [@@deriving sexp_of]

  include Timing.Timed0 with type t := t

  val files : t -> (string, File.t) List.Assoc.t
  (** [files c] gets an associative list of all analysis results for files
      on compiler [c]. *)

  val make :
       ?time_taken:Time.Span.t
    -> files:(string, File.t) List.Assoc.t
    -> unit
    -> t
  (** [make ?time_taken ~files ()] creates a compiler analysis given file
      analyses [files] and total time taken in compiler [time_taken]. *)
end

module Machine : sig
  (** [t] is the opaque type of a machine analysis. *)
  type t [@@deriving sexp_of]

  include Timing.Timed0 with type t := t

  val compilers : t -> (Config.Id.t, Compiler.t) List.Assoc.t
  (** [compilers m] gets an associative list of all analysis results for
      compilers on machine [m]. *)

  val files : t -> (Config.Id.t * string * File.t) list
  (** [files m] gets a list of (compiler ID, filename, analysis) tuples for
      all files processed on machine [m]. *)

  val make :
       ?time_taken:Time.Span.t
    -> compilers:(Config.Id.t, Compiler.t) List.Assoc.t
    -> unit
    -> t
  (** [make ?time_taken ~compilers ()] creates a compiler analysis given
      compiler analyses [compilers] and total time taken on machine
      [time_taken]. *)
end

(** [t] is the opaque type of a full analysis. *)
type t [@@deriving sexp_of]

val make :
     ?time_taken:Time.Span.t
  -> machines:(Config.Id.t, Machine.t) List.Assoc.t
  -> unit
  -> t
(** [make ?time_taken ~machines ()] creates a top-level analysis given
    machine analyses [machines] and total time taken [time_taken]. *)

(** Rows combine a file analysis with all information about the machine,
    compiler, and file to which it belongs. *)
module Row : sig
  (** Opaque type of rows. *)
  type 'a t [@@deriving sexp_of]

  val machine_id : _ t -> Config.Id.t
  (** [machine_id row] gets the ID of the machine of [row]. *)

  val compiler_id : _ t -> Config.Id.t
  (** [compiler_id row] gets the ID of the compiler of [row]. *)

  val filename : _ t -> string
  (** [filename row] gets the name of the file [row] represents. *)

  val analysis : 'a t -> 'a
  (** [analysis row] gets the analysis for [row]'s file. *)

  (** {3 Consuming specific types of row} *)

  val deviations : File.t t -> State_deviation.t t option
  (** [deviations row] sees whether [row] discusses a file where there were
      state set deviations; if so, it returns those deviations as a new row. *)

  val to_table_row : File.t t -> Tabulator.row
  (** [to_table_row row] converts a [row] containing file analysis to a
      tabulator row. *)
end

val file_rows : t -> File.t Row.t list
(** [file_rows x] gets a list of (machine ID, compiler ID, filename,
    analysis) rows for all files in analysis [x]. *)

include Tabulator.Tabular with type data := t

val deviation_rows : t -> State_deviation.t Row.t list
(** [deviation_rows x] gets a list of (machine ID, compiler ID, filename,
    deviation) rows for all files in analysis [x] where there are deviations
    in the state analysis. *)

val machines : t -> (Config.Id.t, Machine.t) List.Assoc.t
(** [machines t] gets an associative list of all analysis results for
    machines in run [t]. *)
