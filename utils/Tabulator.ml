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

open Core

type row = (Format.formatter -> unit) list

(** Internally, rows can be either printer lists (as above) or
    dividers. *)
type inner_row =
  | Data    of row
  | Divider of char
;;

type t =
  { header        : row
  ; sep           : string
  ; terminator    : string option
  ; rows_reversed : inner_row list
  ; column_widths : int list
  }
;;

let pp_cell_contents f pp = pp f

let stride cell =
  String.length (Format.asprintf "@[<h>%a@]" pp_cell_contents cell)
;;

let make ?(sep="  ") ?terminator ~header () =
  let open Or_error.Let_syntax in
  let%map () = Result.ok_if_true (not (List.is_empty header))
      ~error:(Error.of_string "Header row must not be empty.")
  in
  { header        = header
  ; sep
  ; terminator
  ; rows_reversed = []
  ; column_widths = List.map ~f:stride header
  }
;;

let column_count t = List.length (t.column_widths)

let update_column_lengths row old_lengths =
  (* We already checked that the new row has the right length. *)
  List.map2_exn
    row
    old_lengths
    ~f:(fun cell old_length -> Int.max old_length (stride cell))
;;

let with_row row t =
  let open Or_error.Let_syntax in
  let row_length = List.length row in
  let expected_length = column_count t in
  let%map () = Result.ok_if_true (Int.equal row_length expected_length)
      ~error:(Error.create_s
                [%message "Header row has wrong length"
                    ~expected:(expected_length : int)
                    ~got:(row_length : int)]
             )
  in
  { t with rows_reversed = (Data row::t.rows_reversed)
         ; column_widths = update_column_lengths row t.column_widths
  }
;;

let with_rows rows t =
  let module Mapper = Fold_map.List.On_monad (Or_error) in
  Mapper.foldM ~f:(Fn.flip with_row) ~init:t rows
;;

let with_rule rule_char t =
  Ok { t with rows_reversed = (Divider rule_char::t.rows_reversed) }
;;

let tabstop = function
  | `Header -> Format.pp_set_tab
  | `Normal -> Format.pp_print_tab
;;

let end_cell mode t f () =
  tabstop mode f ();
  String.pp f t.sep
;;

let pp_terminator mode t f =
  Option.iter t.terminator
    ~f:(fun term -> tabstop mode f (); String.pp f term)
;;

let end_line mode t f () =
  pp_terminator mode t f;
  Format.pp_print_tab f ()
;;

let pp_header_cell f (column_width, cell) =
  (* Since we're setting tabs here, we need to pad the column width. *)
  let cell_stride = stride cell in
  let padding = Int.max 0 (column_width - cell_stride) in
  Format.fprintf f "@[<h>%a%s@]"
    pp_cell_contents cell
    (String.init padding ~f:(Fn.const ' '));
;;

let pp_cell f cell =
  Format.fprintf f "@[<h>%a@]" pp_cell_contents cell;
;;

let pp_header t f header =
  Format.pp_set_tab f (); (* initial tabstop *)
  let columns = List.zip_exn t.column_widths header in
  Format.pp_print_list ~pp_sep:(end_cell `Header t)
    pp_header_cell f columns;
;;

let pp_data_row t =
  Format.pp_print_list ~pp_sep:(end_cell `Normal t) pp_cell

let pp_divider t f divider_char =
  let div f width =
    String.pp f (String.init width ~f:(Fn.const divider_char))
  in
  let div_sep f () =
    tabstop `Normal f ();
    div f (String.length t.sep)
  in
  Format.pp_print_list ~pp_sep:div_sep div f t.column_widths
;;

let pp_row t f = function
  | Data    r -> pp_data_row t f r
  | Divider c -> pp_divider  t f c
;;

let pp_rows t =
  Format.pp_print_list ~pp_sep:(end_line `Normal t) (pp_row t)
;;

let pp f t =
  Format.pp_open_tbox f ();
  pp_header t f t.header;
  if List.is_empty t.rows_reversed
  then pp_terminator `Header t f
  else begin
    end_line `Header t f ();
    pp_rows t f (List.rev t.rows_reversed);
    pp_terminator `Normal t f
  end;
  Format.pp_close_tbox f ()
;;

let%expect_test "Sample use of tabulator with 'with_rows'" =
  let tab_result =
    Result.(
      make
        ~header:(List.map
                   ["Item"; "Quantity"; "Available"]
                   ~f:(Fn.flip String.pp))
        ()
      >>= with_rows
        [ [ Fn.flip String.pp            "Bicycle"
          ; Fn.flip Int.pp               5
          ; Fn.flip Format.pp_print_bool false
          ]
        ; [ Fn.flip String.pp            "Car"
          ; Fn.flip Int.pp               10
          ; Fn.flip Format.pp_print_bool true
          ]
        ; [ Fn.flip String.pp            "African Swallow"
          ; Fn.flip Int.pp               1
          ; Fn.flip Format.pp_print_bool true
          ]
        ]
      >>| pp Format.std_formatter
    ) in
  Format.print_newline ();
  Format.printf "@[%a@]@." Sexp.pp_hum ([%sexp_of: unit Or_error.t] tab_result);
  [%expect {|
    Item             Quantity  Available
    Bicycle          5         false
    Car              10        true
    African Swallow  1         true
    (Ok ()) |}]
;;


let%expect_test "Sample use of tabulator with rules" =
  let tab_result =
    Result.(
      make
        ~header:(List.map
                   ["Item"; "Quantity"; "Available"]
                   ~f:(Fn.flip String.pp))
        ()
      >>= with_rule '='
      >>= with_row
        [ Fn.flip String.pp            "Bicycle"
        ; Fn.flip Int.pp               5
        ; Fn.flip Format.pp_print_bool false
        ]
      >>= with_row
        [ Fn.flip String.pp            "Car"
        ; Fn.flip Int.pp               10
        ; Fn.flip Format.pp_print_bool true
        ]
      >>= with_rule '-'
      >>= with_row
        [ Fn.flip String.pp            "African Swallow"
        ; Fn.flip Int.pp               1
        ; Fn.flip Format.pp_print_bool true
        ]
      >>| pp Format.std_formatter
    ) in
  Format.print_newline ();
  Format.printf "@[%a@]@." Sexp.pp_hum ([%sexp_of: unit Or_error.t] tab_result);
  [%expect {|
    Item             Quantity  Available
    ====================================
    Bicycle          5         false
    Car              10        true
    ------------------------------------
    African Swallow  1         true
    (Ok ()) |}]
;;

let%expect_test "Sample use of tabulator with custom separators" =
  let tab_result =
    Result.(
      make
        ~sep:" | "
        ~terminator:";"
        ~header:(List.map
                   ["Item"; "Quantity"; "Available"]
                   ~f:(Fn.flip String.pp))
        ()
      >>= with_row
        [ Fn.flip String.pp            "Bicycle"
        ; Fn.flip Int.pp               5
        ; Fn.flip Format.pp_print_bool false
        ]
      >>= with_row
        [ Fn.flip String.pp            "Car"
        ; Fn.flip Int.pp               10
        ; Fn.flip Format.pp_print_bool true
        ]
      >>= with_row
        [ Fn.flip String.pp            "African Swallow"
        ; Fn.flip Int.pp               1
        ; Fn.flip Format.pp_print_bool true
        ]
      >>| pp Format.std_formatter
    ) in
  Format.print_newline ();
  Format.printf "@[%a@]@." Sexp.pp_hum ([%sexp_of: unit Or_error.t] tab_result);
  [%expect {|
    Item            | Quantity | Available;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    African Swallow | 1        | true     ;
    (Ok ()) |}]
;;

let%expect_test "Sample use of tabulator with custom separators and rule" =
  let tab_result =
    Result.(
      make
        ~sep:" | "
        ~terminator:";"
        ~header:(List.map
                   ["Item"; "Quantity"; "Available"]
                   ~f:(Fn.flip String.pp))
        ()
      >>= with_rule '='
      >>= with_row
        [ Fn.flip String.pp            "Bicycle"
        ; Fn.flip Int.pp               5
        ; Fn.flip Format.pp_print_bool false
        ]
      >>= with_row
        [ Fn.flip String.pp            "Car"
        ; Fn.flip Int.pp               10
        ; Fn.flip Format.pp_print_bool true
        ]
      >>= with_rule '-'
      >>= with_row
        [ Fn.flip String.pp            "African Swallow"
        ; Fn.flip Int.pp               1
        ; Fn.flip Format.pp_print_bool true
        ]
      >>| pp Format.std_formatter
    ) in
  Format.print_newline ();
  Format.printf "@[%a@]@." Sexp.pp_hum ([%sexp_of: unit Or_error.t] tab_result);
  [%expect {|
    Item            | Quantity | Available;
    ======================================;
    Bicycle         | 5        | false    ;
    Car             | 10       | true     ;
    --------------------------------------;
    African Swallow | 1        | true     ;
    (Ok ()) |}]
;;

module type Tabular = sig
  type data
  val to_table : data -> t Or_error.t
end

module type Tabular_extensions = sig
  type data
  val pp_as_table
    :  ?on_error : (Format.formatter -> Error.t -> unit)
    -> Format.formatter
    -> data
    -> unit
  ;;
end

module Extend_tabular (T : Tabular)
  : Tabular_extensions with type data := T.data = struct
  let pp_error_default f error =
    Format.fprintf f "@[<error building table: %a>@]"
      Error.pp error
  ;;

  let pp_as_table ?(on_error=pp_error_default) f t =
    match T.to_table t with
    | Ok    table -> pp f table
    | Error e     -> on_error f e
  ;;
end
