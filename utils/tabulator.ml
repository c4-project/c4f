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

open Base
open Stdio

type row = string list

module Column = struct
  type cell = Data of string | Rule of char

  type t = {cells_reversed: cell list; width: int}

  let init (width : int) : t = {cells_reversed= []; width}

  let height (column : t) : int = List.length column.cells_reversed

  let add_cell (column : t) ~(data : string) : t =
    { width= Int.max column.width (String.length data)
    ; cells_reversed= Data data :: column.cells_reversed }

  let add_rule (column : t) ~(char : char) : t =
    {column with cells_reversed= Rule char :: column.cells_reversed}

  let cell_to_text_block (column : t) : cell -> Text_block.t = function
    | Data s ->
        Text_block.text s
    | Rule char ->
        Text_block.fill char ~width:column.width ~height:1

  let to_text_block (column : t) : Text_block.t =
    column.cells_reversed
    |> List.rev_map ~f:(cell_to_text_block column)
    |> Text_block.vcat
end

type t =
  { header: row
  ; sep: string
  ; terminator: string option
  ; columns: Column.t list }
[@@deriving fields]

let length_at (index : int) (header : row) : int =
  index |> List.nth header |> Option.value_map ~f:String.length ~default:0

let make ?(sep = "  ") ?terminator ~header () =
  let open Or_error.Let_syntax in
  let%map () =
    Result.ok_if_true
      (not (List.is_empty header))
      ~error:(Error.of_string "Header row must not be empty.")
  in
  (* Easiest way to handle separators. *)
  let header = List.intersperse ~sep header in
  { header
  ; sep
  ; terminator
  ; columns=
      List.init (List.length header) ~f:(fun i ->
          Column.init (length_at i header) ) }

let validate_length (table : t) : row Validate.check =
  Validate.booltest
    (Travesty.T_fn.on List.length Int.equal table.header)
    ~if_false:"Row length inconsistency."

let add_row_cells (column : Column.t) (data : string) : Column.t =
  Column.add_cell column ~data

let add_row_inner (columns : Column.t list) (row : row) :
    Column.t list List.Or_unequal_lengths.t =
  List.map2 columns row ~f:add_row_cells

let add_row (table : t) ~(row : row) : t Or_error.t =
  let sepped_row = List.intersperse ~sep:(sep table) row in
  let open Or_error.Let_syntax in
  let%bind () = Validate.result (validate_length table sepped_row) in
  let%map columns =
    match add_row_inner table.columns sepped_row with
    | Ok c ->
        Or_error.return c
    | Unequal_lengths ->
        Or_error.error_string "Row length inconsistency (columns <> row)."
  in
  {table with columns}

let add_rows (table : t) ~(rows : row list) : t Or_error.t =
  Travesty.T_list.With_errors.fold_m
    ~f:(fun table' row -> add_row table' ~row)
    ~init:table rows

let add_rule ?(char : char = '-') (table : t) : t Or_error.t =
  let columns = List.map ~f:(Column.add_rule ~char) table.columns in
  Ok {table with columns}

module Print = struct
  let terminator_height (table : t) : int =
    let max_column_height =
      Travesty.T_list.max_measure ~measure:Column.height (columns table)
    in
    max_column_height + 1

  (* for the header *)

  let string_to_terminator (height : int) (str : string) : Text_block.t =
    let slices = List.init height ~f:(Fn.const (Text_block.text str)) in
    Text_block.vcat slices

  let make_terminator (table : t) : Text_block.t list =
    table |> terminator |> Option.to_list
    |> List.map ~f:(string_to_terminator (terminator_height table))

  let make_block (table : t) : Text_block.t =
    let columns = List.map ~f:Column.to_text_block (columns table) in
    let headered_columns =
      List.map2_exn (header table) columns ~f:(fun h_cell column ->
          Text_block.(vcat [text h_cell; column]) )
    in
    Text_block.hcat (headered_columns @ make_terminator table)

  let table ?(oc : Out_channel.t = Out_channel.stdout) (table : t) : unit =
    let block = make_block table in
    let rendered = Text_block.render block in
    Out_channel.output_string oc rendered
end

let print = Print.table

let%expect_test "Sample use of tabulator with 'add_rows'" =
  let tab_result =
    Result.(
      make ~header:["Item"; "Quantity"; "Available"] ()
      >>= add_rows
            ~rows:
              [ ["Bicycle"; "5"; "false"]
              ; ["Car"; "10"; "true"]
              ; ["African Swallow"; "1"; "true"] ]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item             Quantity  Available
    Bicycle          5         false
    Car              10        true
    African Swallow  1         true
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with rules" =
  let tab_result =
    Result.(
      make ~header:["Item"; "Quantity"; "Available"] ()
      >>= add_rule ~char:'='
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_rule ~char:'-'
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item             Quantity  Available
    ====================================
    Bicycle          5         false
    Car              10        true
    ------------------------------------
    African Swallow  1         true
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with custom separators" =
  let tab_result =
    Result.(
      make ~sep:" | " ~terminator:";"
        ~header:["Item"; "Quantity"; "Available"]
        ()
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item            | Quantity | Available;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    African Swallow | 1        | true     ;
    (Ok ()) |}]

let%expect_test "Sample use of tabulator with custom separators and rule" =
  let tab_result =
    Result.(
      make ~sep:" | " ~terminator:";"
        ~header:["Item"; "Quantity"; "Available"]
        ()
      >>= add_rule ~char:'='
      >>= add_row ~row:["Bicycle"; "5"; "false"]
      >>= add_row ~row:["Car"; "10"; "true"]
      >>= add_rule ~char:'-'
      >>= add_row ~row:["African Swallow"; "1"; "true"]
      >>| print)
  in
  print_s [%sexp (tab_result : unit Or_error.t)] ;
  [%expect
    {|
    Item            | Quantity | Available;
    ======================================;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    --------------------------------------;
    African Swallow | 1        | true     ;
    (Ok ()) |}]

module type Tabular = sig
  type data

  val to_table : data -> t Or_error.t
end

module type Tabular_extensions = sig
  type data

  val print_as_table :
    ?oc:Out_channel.t -> ?on_error:(Error.t -> unit) -> data -> unit
end

module Extend_tabular (T : Tabular) :
  Tabular_extensions with type data := T.data = struct
  let error_default error =
    Fmt.epr "@[<error building table: %a>@]" Error.pp error

  let print_as_table ?(oc : Out_channel.t option)
      ?(on_error = error_default) t =
    match T.to_table t with
    | Ok table ->
        print ?oc table
    | Error e ->
        on_error e
end
