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

open Core_kernel
open Utils

type t = string [@@deriving sexp, equal]

module Set = My_set.Extend (Set.Make (String))

module Sort = struct
  module M = struct
    type t =
      | Jump
      | Heap
      | Label
    [@@deriving enum, sexp]

    let table =
      [ Jump,  "jump"
      ; Heap,  "heap"
      ; Label, "label"
      ]
  end

  include M
  include Enum.Extend_table (M)
end

module Table = struct
  type elt = t

  (* Not necessarily an associative list: each symbol might be
     in multiple different sort buckets. *)
  type nonrec t = (t * Sort.t) list

  let empty = []

  let of_sets =
    List.concat_map
      ~f:(fun (set, sort) ->
          List.map ~f:(fun sym -> (sym, sort))
            (Set.to_list set)
        )
  ;;

  let add tbl sym sort = (sym, sort) :: tbl

  let remove tbl sym sort =
    Travesty.T_list.exclude
      ~f:(Tuple2.equal ~eq1:String.equal ~eq2:Sort.equal (sym, sort))
      tbl
  ;;

  let set_of_sorts tbl sorts =
    tbl
    |> List.filter_map
      ~f:(fun (sym, sort) ->
          if Sort.Set.mem sorts sort then Some sym else None)
    |> Set.of_list
  ;;

  let set_of_sort tbl sort = set_of_sorts tbl (Sort.Set.singleton sort)

  let set tbl = tbl |> List.map ~f:fst |> Set.of_list

  let mem tbl ?sort symbol =
    let actual_sort =
      List.Assoc.find tbl ~equal:String.equal symbol
    in
    Option.is_some actual_sort
    && (Option.is_none sort
        || Option.equal Sort.equal sort actual_sort)
  ;;

  module Tabulate : Tabulator.Tabular with type data = t = struct
    type data = t

    let to_row (symbol, sorts) =
      [ Fn.flip String.pp symbol
      ; Fn.flip Sort.pp sorts
      ]
    ;;

    let to_table table =
      let open Result.Monad_infix in
      Tabulator.(
        make
          ~header:(List.map ["Symbol"; "Sort"] ~f:(Fn.flip String.pp))
          ~sep:" | "
          ()
        >>= with_rule '='
        >>= with_rows (List.map ~f:to_row table)
      )
  end

  include (Tabulate : Tabulator.Tabular with type data := t)
  include Tabulator.Extend_tabular (Tabulate)

  let%expect_test "Printing symbol table using tabulator" =
    Format.printf "@[%a@]@." (fun f -> pp_as_table f)
      Sort.[ "foo", Heap
           ; "bar", Heap
           ; "bar", Label
           ; "baz", Jump
           ];
    [%expect {|
      Symbol | Sort
      ==============
      foo    | heap
      bar    | heap
      bar    | label
      baz    | jump |}]
  ;;
end

module Flag = Flag_enum.None
