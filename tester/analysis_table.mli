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

(** Outputting test analyses in tabular formats. *)

open Utils

(** Enumeration of levels of 'interestingness', which determine which rows
    appear in certain analysis tables. *)
module Interest_level : sig
  (** Type of interest levels. *)
  type t =
    | All  (** Interested in all rows. *)
    | Deviations  (** Interested in deviations, UB, and errors. *)
    | Ub  (** Interested in undefined behaviour and errors. *)
    | Errors  (** Interested in errors only. *)

  (** An analysis item associated with the interestingness threshold that
      should be used when tabulating it. *)
  type 'a filtered = {data: 'a; level: t}
end

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

  (** {3 Converting rows to table format} *)

  val to_table_row : 'a t -> f:('a -> string list) -> Tabulator.row
  (** [to_table_row row ~f] converts [row] to a tabulator row, using [f] to
      flatten the analysis into a list of cells. *)
end

(** Tabulating an analysis by pulling all data from each 'interesting' file
    run. *)
module On_files : sig
  val rows : Analysis.t -> Analysis.File.t Row.t list
  (** [rows x] gets a list of (machine ID, compiler ID, filename, analysis)
      rows for all files in analysis [x]. *)

  (** We can tabulate a pair of analysis and interestingness threshold. *)
  type data = Analysis.t Interest_level.filtered

  include Tabulator.Tabular with type data := data

  include Tabulator.Tabular_extensions with type data := data
end

module On_deviations : sig
  val rows : Analysis.t -> Lib.Sim_diff.Order.t Row.t list
  (** [rows x] gets a list of (machine ID, compiler ID, filename, deviation)
      rows for all files in analysis [x] where there are deviations in the
      state analysis. *)

  (** We can tabulate an analysis (interestingness is irrelevant here). *)
  type data = Analysis.t

  include Tabulator.Tabular with type data := data

  include Tabulator.Tabular_extensions with type data := data
end
