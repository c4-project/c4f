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
open Act_common
open Act_utils

include module type of Analysis_intf

module Herd : sig
  (** [t] is the type of Herd analysis runs. *)
  type t =
    | Run of Act_sim.Diff.t
        (** Herd ran on both sides, with the given differential result. *)
    | Disabled  (** Herd was disabled. *)
    | Errored of [`C | `Assembly]  (** Herd encountered an error. *)

  include Pretty_printer.S with type t := t

  include Sim_predicates with type t := t
end

(** A file analysis. *)
module File : sig
  (** [t] is the opaque type of a single-file analysis. *)
  type t

  include Timing.Timed0 with type t := t

  val herd : t -> Herd.t
  (** [herd f] gets the Herd run for file [f]. *)

  val make :
       ?time_taken:Time.Span.t
    -> ?time_taken_in_cc:Time.Span.t
    -> herd:Herd.t
    -> unit
    -> t
  (** [make ?time_taken ?time_taken_in_cc ()] creates a file analysis given
      Herd run [herd], total time taken [time_taken], and time taken in the
      C compiler [time_taken_in_cc]. *)

  val time_taken_in_cc : t -> Time.Span.t option
  (** [time_taken_in_cc file] gets an estimate of the time spent compiling
      file [file], if any is available. *)

  val state_set_order : t -> Act_sim.Diff.Order.t option
  (** [state_set_order file] gets the partial ordering between the file's C
      and assembly state sets, provided that the file successfully underwent
      testing. *)

  include Sim_predicates with type t := t
end

(** A compiler analysis, containing multiple file analyses. *)
module Compiler : sig
  (** [t] is the opaque type of a single-compiler analysis. *)
  type t

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

(** A machine analysis, containing multiple compiler analyses. *)
module Machine : sig
  (** Opaque type of a machine analysis. *)
  type t

  include Timing.Timed0 with type t := t

  val compilers : t -> (Id.t, Compiler.t) List.Assoc.t
  (** [compilers m] gets an associative list of all analysis results for
      compilers on machine [m]. *)

  val files : t -> (Id.t * string * File.t) list
  (** [files m] gets a list of (compiler ID, filename, analysis) tuples for
      all files processed on machine [m]. *)

  val make :
       ?time_taken:Time.Span.t
    -> compilers:(Id.t, Compiler.t) List.Assoc.t
    -> unit
    -> t
  (** [make ?time_taken ~compilers ()] creates a compiler analysis given
      compiler analyses [compilers] and total time taken on machine
      [time_taken]. *)
end

(** [t] is the opaque type of a full analysis. *)
type t

val make :
     ?time_taken:Time.Span.t
  -> machines:(Id.t, Machine.t) List.Assoc.t
  -> unit
  -> t
(** [make ?time_taken ~machines ()] creates a top-level analysis given
    machine analyses [machines] and total time taken [time_taken]. *)

val machines : t -> (Id.t, Machine.t) List.Assoc.t
(** [machines t] gets an associative list of all analysis results for
    machines in run [t]. *)
