(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
module Tx = Travesty_base_exts
open Act_utils
module M = String
include M

module Sort = struct
  module M = struct
    type t = Jump | Heap | Label [@@deriving enum, sexp]

    let table = [(Jump, "jump"); (Heap, "heap"); (Label, "label")]
  end

  include M
  include Enum.Extend_table (M)
end

module Table = struct
  type elt = t

  (* Not necessarily an associative list: each symbol might be in multiple
     different sort buckets. *)
  type t = (M.t * Sort.t) list

  let empty : t = []

  let of_sets : (Set.M(M).t * Sort.t) list -> t =
    List.concat_map ~f:(fun (set, sort) ->
        List.map ~f:(fun sym -> (sym, sort)) (Set.to_list set))

  let add (tbl : t) (sym : M.t) (sort : Sort.t) : t = (sym, sort) :: tbl

  let remove (tbl : t) (sym : M.t) (sort : Sort.t) =
    Tx.List.exclude ~f:([%equal: M.t * Sort.t] (sym, sort)) tbl

  let set_of_sorts (tbl : t) (sorts : Set.M(Sort).t) : Set.M(M).t =
    tbl
    |> List.filter_map ~f:(fun (sym, sort) ->
           if Set.mem sorts sort then Some sym else None)
    |> Set.of_list (module M)

  let set_of_sort (tbl : t) (sort : Sort.t) : Set.M(M).t =
    set_of_sorts tbl (Set.singleton (module Sort) sort)

  let set (tbl : t) : Set.M(M).t =
    tbl |> List.map ~f:fst |> Set.of_list (module M)

  let mem (tbl : t) ?(sort : Sort.t option) (symbol : M.t) : bool =
    let actual_sort = List.Assoc.find tbl ~equal:String.equal symbol in
    Option.is_some actual_sort
    && (Option.is_none sort || Option.equal Sort.equal sort actual_sort)

  module Tabulate : Tabulator.Tabular with type data = t = struct
    type data = t

    let to_row (symbol, sorts) = [symbol; Sort.to_string sorts]

    let to_table table =
      let open Result.Monad_infix in
      Tabulator.(
        make ~header:["Symbol"; "Sort"] ~sep:" | " ()
        >>= add_rule ~char:'='
        >>= add_rows ~rows:(List.map ~f:to_row table))
  end

  include (Tabulate : Tabulator.Tabular with type data := t)

  include Tabulator.Extend_tabular (Tabulate)

  let%expect_test "Printing symbol table using tabulator" =
    print_as_table
      Sort.[("foo", Heap); ("bar", Heap); ("bar", Label); ("baz", Jump)] ;
    [%expect
      {|
      Symbol | Sort
      ==============
      foo    | heap
      bar    | heap
      bar    | label
      baz    | jump |}]
end

module Flag = Flag_enum.None
