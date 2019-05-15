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

open Core_kernel (* not Base: we need Time.Span. *)

module Tx = Travesty_core_kernel_exts
open Act_common
open Act_utils
module A = Analysis

module Interest_level = struct
  type t = All | Deviations | Ub | Errors

  type 'a filtered = {data: 'a; level: t}
end

let pp_span_opt = Fmt.(option ~none:(unit "-") Time.Span.pp)

module Row = struct
  type 'a t =
    {machine_id: Id.t; compiler_id: Id.t; filename: string; analysis: 'a}
  [@@deriving sexp_of, fields, make]

  let to_table_row (type a) (row : a t) ~(f : a -> string list) :
      Tabulator.row =
    [ Fmt.strf "%a" Id.pp row.machine_id
    ; Fmt.strf "%a" Id.pp row.compiler_id
    ; row.filename ]
    @ f row.analysis

  let with_analysis (row : _ t) ~(analysis : 'a) : 'a t =
    make ~machine_id:(machine_id row) ~compiler_id:(compiler_id row)
      ~filename:(filename row) ~analysis

  let filter_map_analysis (row : 'a t) ~(f : 'a -> 'b option) : 'b t option
      =
    Option.Let_syntax.(
      let%map analysis = f row.analysis in
      with_analysis row ~analysis)
end

let file_rows_of_machine (machine_id : Id.t) (machine : A.Machine.t) :
    A.File.t Row.t list =
  List.map (A.Machine.files machine)
    ~f:(fun (compiler_id, filename, analysis) ->
      Row.make ~machine_id ~compiler_id ~filename ~analysis )

let machine_rule = '='

let compiler_rule = '-'

(** [maybe_with_rule last_mid this_mid last_cid this_cid tabulator]
    determines, by checking whether the machine or compiler IDs have changed
    from last row (if there was a last row), whether to insert a rule on
    [tabulator] and, if so, which one to insert. *)
let maybe_with_rule last_mid this_mid last_cid this_cid tabulator =
  match (last_mid, last_cid) with
  | None, _ | _, None ->
      (* Assume we're on the first row *)
      Tabulator.add_rule ~char:machine_rule tabulator
  | Some lmid, Some lcid ->
      if Id.equal lmid this_mid then
        if Id.equal lcid this_cid then Or_error.return tabulator
          (* no rule *)
        else Tabulator.add_rule ~char:compiler_rule tabulator
      else Tabulator.add_rule ~char:machine_rule tabulator

module type Basic_tabulator = sig
  (** Type of analysis. *)
  type analysis

  (** Type of the raw input to the tabulator. *)
  type data

  val rows : data -> analysis Row.t list
  (** [rows d] should get the list of [Row]s from [d]. *)

  val analysis_header : string list
  (** [analysis_header] should be a list of headings for the cells produced
      by [analysis_to_cells]. *)

  val analysis_to_cells : analysis -> string list
  (** [analysis_to_cells a] should generate table cells for [a]. *)
end

let common_header : string list = ["Machine"; "Compiler"; "File"]

module Make_tabulator (B : Basic_tabulator) :
  Tabulator.Tabular with type data = B.data = struct
  type data = B.data

  let header : string list = common_header @ B.analysis_header

  let with_file (last_mid, last_cid, tabulator)
      (analysis_row : B.analysis Row.t) :
      (Id.t option * Id.t option * Tabulator.t) Or_error.t =
    let mid = Row.machine_id analysis_row in
    let cid = Row.compiler_id analysis_row in
    let row = Row.to_table_row analysis_row ~f:B.analysis_to_cells in
    Or_error.(
      return tabulator
      >>= maybe_with_rule last_mid mid last_cid cid
      >>= Tabulator.add_row ~row
      >>| fun t' -> (Some mid, Some cid, t'))

  let with_files (rows : B.analysis Row.t list) (tab : Tabulator.t) :
      Tabulator.t Or_error.t =
    Or_error.(
      rows
      |> Tx.List.With_errors.fold_m ~init:(None, None, tab) ~f:with_file
      >>| Tuple3.get3)

  let to_table (a : data) : Tabulator.t Or_error.t =
    Or_error.(Tabulator.make ~header () >>= with_files (B.rows a))
end

module On_files = struct
  let rows (a : A.t) : A.File.t Row.t list =
    List.concat_map (A.machines a) ~f:(Tuple2.uncurry file_rows_of_machine)

  let has_ub_and_below : Analysis.File.t -> bool =
    Analysis.File.(Tx.Fn.(has_deviations ||| is_undefined))

  let has_deviations_and_below : Analysis.File.t -> bool =
    Analysis.File.(Tx.Fn.(has_ub_and_below ||| has_errors))

  let is_file_interesting : Interest_level.t -> Analysis.File.t -> bool =
    function
    | All ->
        Fn.const true
    | Deviations ->
        has_deviations_and_below
    | Ub ->
        has_ub_and_below
    | Errors ->
        Analysis.File.has_errors

  let is_interesting (il : Interest_level.t) (row : A.File.t Row.t) : bool =
    is_file_interesting il (Row.analysis row)

  let interesting_rows (a : A.t Interest_level.filtered) :
      A.File.t Row.t list =
    a.data |> rows |> List.filter ~f:(is_interesting a.level)

  module M :
    Tabulator.Tabular with type data = A.t Interest_level.filtered =
  Make_tabulator (struct
    type data = A.t Interest_level.filtered

    type analysis = A.File.t

    let rows = interesting_rows

    let analysis_header : string list = ["Result"; "Time/total"; "Time/CC"]

    let analysis_to_cells (file : A.File.t) : string list =
      Fmt.(
        List.map
          ~f:(fun pp -> strf "%a" pp file)
          [ using A.File.herd A.Herd.pp
          ; using A.File.time_taken pp_span_opt
          ; using A.File.time_taken_in_cc pp_span_opt ])
  end)

  include M
  include Tabulator.Extend_tabular (M)
end

module On_deviations = struct
  let deviating_order_opt (file : A.File.t) : Act_sim.Diff.Order.t option =
    Tx.Option.(
      file |> A.File.state_set_order
      |> exclude ~f:Act_sim.Diff.Order.is_equal)

  let row_deviating_order_opt :
      A.File.t Row.t -> Act_sim.Diff.Order.t Row.t option =
    Row.filter_map_analysis ~f:deviating_order_opt

  let rows (a : A.t) : Act_sim.Diff.Order.t Row.t list =
    a |> On_files.rows |> List.filter_map ~f:row_deviating_order_opt

  module M : Tabulator.Tabular with type data = A.t = Make_tabulator (struct
    type data = A.t

    type analysis = Act_sim.Diff.Order.t

    let rows = rows

    let analysis_header : string list =
      ["Num. in C only"; "Num. in ASM only"]

    let analysis_to_cells (ord : Act_sim.Diff.Order.t) : string list =
      [ Int.to_string (Set.length (Act_sim.Diff.Order.in_left_only ord))
      ; Int.to_string (Set.length (Act_sim.Diff.Order.in_right_only ord)) ]
  end)

  include M
  include Tabulator.Extend_tabular (M)
end
